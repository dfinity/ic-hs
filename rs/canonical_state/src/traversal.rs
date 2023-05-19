use crate::lazy_tree::LazyTree;
use crate::visitor::{Control, Visitor};
use ic_replicated_state::ReplicatedState;

/// Traverses lazy tree using specified visitor.
fn traverse_lazy_tree<'a, V: Visitor>(t: &LazyTree<'a>, v: &mut V) -> Result<(), V::Output> {
    match t {
        LazyTree::Blob(b, _) => v.visit_blob(b),
        LazyTree::LazyBlob(thunk) => {
            let b = thunk();
            v.visit_blob(&b)
        }
        LazyTree::LazyFork(f) => {
            v.start_subtree()?;
            for (l, t) in f.children() {
                match v.enter_edge(l.as_bytes())? {
                    Control::Skip => continue,
                    Control::Continue => {
                        traverse_lazy_tree(&t, v)?;
                    }
                }
            }
            v.end_subtree()
        }
    }
}

/// Traverses `state` as if it was a state in canonical form using visitor `v`.
///
/// By supplying different visitors, one can use `traverse` to serialize the
/// `state` for transmitting it over the network to another replica, compute a
/// hash tree for certification or extract a specific value.
pub fn traverse<V: Visitor>(state: &ReplicatedState, mut v: V) -> V::Output {
    let t = LazyTree::from(state);
    match traverse_lazy_tree(&t, &mut v) {
        Err(output) => output,
        _ => v.finish(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        encoding::{encode_stream_header, types::SystemMetadata, CborProxyEncoder},
        subtree_visitor::{Pattern, SubtreeVisitor},
        test_visitors::{NoopVisitor, TraceEntry as E, TracingVisitor},
        CertificationVersion,
    };
    use ic_base_types::{NumBytes, NumSeconds};
    use ic_certification_version::CURRENT_CERTIFICATION_VERSION;
    use ic_registry_routing_table::{CanisterIdRange, RoutingTable};
    use ic_registry_subnet_features::SubnetFeatures;
    use ic_registry_subnet_type::SubnetType;
    use ic_replicated_state::{
        canister_state::{
            execution_state::{CustomSection, CustomSectionType, WasmBinary, WasmMetadata},
            ExecutionState, ExportedFunctions, Global, NumWasmPages,
        },
        metadata_state::SubnetTopology,
        page_map::PageMap,
        testing::ReplicatedStateTesting,
        Memory,
    };
    use ic_test_utilities::{
        mock_time,
        state::new_canister_state,
        types::ids::{canister_test_id, subnet_test_id, user_test_id},
    };
    use ic_types::{CanisterId, Cycles, ExecutionRound};
    use ic_wasm_types::CanisterModule;
    use maplit::btreemap;
    use std::collections::{BTreeSet, VecDeque};
    use std::convert::TryFrom;
    use std::sync::Arc;
    use std::time::Duration;

    const INITIAL_CYCLES: Cycles = Cycles::new(1 << 36);

    fn edge<L: AsRef<[u8]>>(label: L) -> E {
        E::EnterEdge(label.as_ref().to_vec())
    }

    fn leb_num(n: u64) -> E {
        let mut buf = Vec::new();
        leb128::write::unsigned(&mut buf, n).unwrap();
        E::VisitBlob(buf)
    }

    fn encode_metadata(metadata: SystemMetadata) -> Vec<u8> {
        SystemMetadata::proxy_encode(metadata).unwrap()
    }

    #[test]
    fn test_traverse_empty_state() {
        let mut state = ReplicatedState::new(subnet_test_id(1), SubnetType::Application);
        state.metadata.certification_version = CURRENT_CERTIFICATION_VERSION;
        let visitor = TracingVisitor::new(NoopVisitor);
        assert_eq!(
            vec![
                E::StartSubtree,
                edge("canister"),
                E::StartSubtree,
                E::EndSubtree, // canisters
                edge("metadata"),
                E::VisitBlob(encode_metadata(SystemMetadata {
                    id_counter: None,
                    prev_state_hash: None
                })),
                edge("request_status"),
                E::StartSubtree,
                E::EndSubtree, // request_status
                edge("streams"),
                E::StartSubtree,
                E::EndSubtree, // streams
                edge("subnet"),
                E::StartSubtree,
                E::EndSubtree, // subnets
                edge("time"),
                leb_num(0),
                E::EndSubtree, // global
            ],
            traverse(&state, visitor).0
        );
    }

    #[test]
    fn test_traverse_canister_empty_execution_state() {
        let canister_id = canister_test_id(2);
        let controller = user_test_id(24);
        let controllers_cbor = {
            let mut cbor = vec![217, 217, 247, 129, 74];
            cbor.extend(controller.get().to_vec());
            cbor
        };
        let canister_state = new_canister_state(
            canister_id,
            controller.get(),
            INITIAL_CYCLES,
            NumSeconds::from(100_000),
        );
        let mut state = ReplicatedState::new(subnet_test_id(1), SubnetType::Application);
        state.put_canister_state(canister_state);

        let visitor = TracingVisitor::new(NoopVisitor);
        assert_eq!(
            vec![
                E::StartSubtree, // global
                edge("canister"),
                E::StartSubtree,
                E::EnterEdge(canister_id.get().into_vec()),
                E::StartSubtree,
                E::EndSubtree, // canister
                E::EndSubtree, // canisters
                edge("metadata"),
                E::VisitBlob(encode_metadata(SystemMetadata {
                    id_counter: Some(0),
                    prev_state_hash: None
                })),
                edge("request_status"),
                E::StartSubtree,
                E::EndSubtree, // request_status
                edge("streams"),
                E::StartSubtree,
                E::EndSubtree, // streams
                edge("subnet"),
                E::StartSubtree,
                E::EndSubtree, // subnets
                edge("time"),
                leb_num(0),
                E::EndSubtree, // global
            ],
            traverse(&state, visitor).0
        );

        // Test new certification version.
        state.metadata.certification_version = CURRENT_CERTIFICATION_VERSION;
        let visitor = TracingVisitor::new(NoopVisitor);
        assert_eq!(
            vec![
                E::StartSubtree, // global
                edge("canister"),
                E::StartSubtree,
                E::EnterEdge(canister_id.get().into_vec()),
                E::StartSubtree,
                edge("controller"),
                E::VisitBlob(controller.get().to_vec()),
                edge("controllers"),
                E::VisitBlob(controllers_cbor),
                E::EndSubtree, // canister
                E::EndSubtree, // canisters
                edge("metadata"),
                E::VisitBlob(encode_metadata(SystemMetadata {
                    id_counter: None,
                    prev_state_hash: None
                })),
                edge("request_status"),
                E::StartSubtree,
                E::EndSubtree, // request_status
                edge("streams"),
                E::StartSubtree,
                E::EndSubtree, // streams
                edge("subnet"),
                E::StartSubtree,
                E::EndSubtree, // subnets
                edge("time"),
                leb_num(0),
                E::EndSubtree, // global
            ],
            traverse(&state, visitor).0
        );
    }

    #[test]
    fn test_traverse_canister_with_execution_state() {
        let canister_id = canister_test_id(2);
        let controller = user_test_id(24);
        let controllers_cbor = {
            let mut cbor = vec![217, 217, 247, 129, 74];
            cbor.extend(controller.get().to_vec());
            cbor
        };
        let mut canister_state = new_canister_state(
            canister_id,
            controller.get(),
            INITIAL_CYCLES,
            NumSeconds::from(100_000),
        );
        let wasm_binary = WasmBinary::new(CanisterModule::new(vec![]));
        let wasm_binary_hash = wasm_binary.binary.module_hash();
        let wasm_memory = Memory::new(PageMap::new_for_testing(), NumWasmPages::from(2));

        let metadata = btreemap! {
            String::from("dummy1") => CustomSection::new(CustomSectionType::Private, vec![0, 2]),
            String::from("dummy2") => CustomSection::new(CustomSectionType::Public, vec![2, 1]),
            String::from("dummy3") => CustomSection::new(CustomSectionType::Public, vec![8, 9]),
        };

        let execution_state = ExecutionState {
            canister_root: "NOT_USED".into(),
            session_nonce: None,
            wasm_binary,
            wasm_memory,
            stable_memory: Memory::new_for_testing(),
            exported_globals: vec![Global::I32(1)],
            exports: ExportedFunctions::new(BTreeSet::new()),
            metadata: WasmMetadata::new(metadata),
            last_executed_round: ExecutionRound::from(0),
        };
        canister_state.execution_state = Some(execution_state);

        let mut state = ReplicatedState::new(subnet_test_id(1), SubnetType::Application);
        state.put_canister_state(canister_state);

        let visitor = TracingVisitor::new(NoopVisitor);
        assert_eq!(
            vec![
                E::StartSubtree,
                edge("canister"),
                E::StartSubtree,
                E::EnterEdge(canister_id.get().into_vec()),
                E::StartSubtree,
                edge("certified_data"),
                E::VisitBlob(vec![]),
                E::EndSubtree, // canister
                E::EndSubtree, // canisters
                edge("metadata"),
                E::VisitBlob(encode_metadata(SystemMetadata {
                    id_counter: Some(0),
                    prev_state_hash: None
                })),
                edge("request_status"),
                E::StartSubtree,
                E::EndSubtree, // request_status
                edge("streams"),
                E::StartSubtree,
                E::EndSubtree, // streams
                edge("subnet"),
                E::StartSubtree,
                E::EndSubtree, // subnets
                edge("time"),
                leb_num(0),
                E::EndSubtree, //global
            ],
            traverse(&state, visitor).0
        );

        // Test new certification version.
        state.metadata.certification_version = CertificationVersion::V2;
        let visitor = TracingVisitor::new(NoopVisitor);
        assert_eq!(
            vec![
                E::StartSubtree,
                edge("canister"),
                E::StartSubtree,
                E::EnterEdge(canister_id.get().into_vec()),
                E::StartSubtree,
                edge("certified_data"),
                E::VisitBlob(vec![]),
                edge("controller"),
                E::VisitBlob(controller.get().to_vec()),
                edge("controllers"),
                E::VisitBlob(controllers_cbor.clone()),
                edge("module_hash"),
                E::VisitBlob(wasm_binary_hash.to_vec()),
                E::EndSubtree, // canister
                E::EndSubtree, // canisters
                edge("metadata"),
                E::VisitBlob(encode_metadata(SystemMetadata {
                    id_counter: Some(0),
                    prev_state_hash: None
                })),
                edge("request_status"),
                E::StartSubtree,
                E::EndSubtree, // request_status
                edge("streams"),
                E::StartSubtree,
                E::EndSubtree, // streams
                edge("subnet"),
                E::StartSubtree,
                E::EndSubtree, // subnets
                edge("time"),
                leb_num(0),
                E::EndSubtree, //global
            ],
            traverse(&state, visitor).0
        );

        // Test new certification version.
        state.metadata.certification_version = CURRENT_CERTIFICATION_VERSION;
        let visitor = TracingVisitor::new(NoopVisitor);
        assert_eq!(
            vec![
                E::StartSubtree,
                edge("canister"),
                E::StartSubtree,
                E::EnterEdge(canister_id.get().into_vec()),
                E::StartSubtree,
                edge("certified_data"),
                E::VisitBlob(vec![]),
                edge("controller"),
                E::VisitBlob(controller.get().to_vec()),
                edge("controllers"),
                E::VisitBlob(controllers_cbor),
                edge("metadata"),
                E::StartSubtree,
                edge("dummy1"),
                E::VisitBlob(vec![0, 2]),
                edge("dummy2"),
                E::VisitBlob(vec![2, 1]),
                edge("dummy3"),
                E::VisitBlob(vec![8, 9]),
                E::EndSubtree, // metadata
                edge("module_hash"),
                E::VisitBlob(wasm_binary_hash.to_vec()),
                E::EndSubtree, // canister
                E::EndSubtree, // canisters
                edge("metadata"),
                E::VisitBlob(encode_metadata(SystemMetadata {
                    id_counter: None,
                    prev_state_hash: None
                })),
                edge("request_status"),
                E::StartSubtree,
                E::EndSubtree, // request_status
                edge("streams"),
                E::StartSubtree,
                E::EndSubtree, // streams
                edge("subnet"),
                E::StartSubtree,
                E::EndSubtree, // subnets
                edge("time"),
                leb_num(0),
                E::EndSubtree, //global
            ],
            traverse(&state, visitor).0
        );
    }

    #[test]
    fn test_traverse_xnet_stream_header() {
        use ic_replicated_state::metadata_state::Stream;
        use ic_types::xnet::{StreamHeader, StreamIndex, StreamIndexedQueue};

        let header = StreamHeader {
            begin: StreamIndex::from(4),
            end: StreamIndex::from(4),
            signals_end: StreamIndex::new(11),
            reject_signals: VecDeque::new(),
        };

        let stream = Stream::new(
            StreamIndexedQueue::with_begin(StreamIndex::from(4)),
            StreamIndex::new(11),
        );

        let mut state = ReplicatedState::new(subnet_test_id(1), SubnetType::Application);
        state.metadata.certification_version = CURRENT_CERTIFICATION_VERSION;
        state.modify_streams(move |streams| {
            streams.insert(subnet_test_id(5), stream);
        });

        let visitor = TracingVisitor::new(NoopVisitor);
        assert_eq!(
            vec![
                E::StartSubtree,
                edge("canister"),
                E::StartSubtree,
                E::EndSubtree, // canisters
                edge("metadata"),
                E::VisitBlob(encode_metadata(SystemMetadata {
                    id_counter: None,
                    prev_state_hash: None
                })),
                edge("request_status"),
                E::StartSubtree,
                E::EndSubtree, // request_status
                edge("streams"),
                E::StartSubtree,
                edge(subnet_test_id(5).get_ref().to_vec()),
                E::StartSubtree,
                edge("header"),
                E::VisitBlob(encode_stream_header(
                    &header,
                    state.metadata.certification_version,
                )),
                edge("messages"),
                E::StartSubtree,
                E::EndSubtree, // messages
                E::EndSubtree, // stream
                E::EndSubtree, // streams
                edge("subnet"),
                E::StartSubtree,
                E::EndSubtree, // subnets
                edge("time"),
                leb_num(0),
                E::EndSubtree, // global
            ],
            traverse(&state, visitor).0
        );
    }

    #[test]
    fn test_traverse_ingress_history() {
        use crate::subtree_visitor::{Pattern, SubtreeVisitor};
        use ic_error_types::{ErrorCode, UserError};
        use ic_test_utilities::types::ids::{message_test_id, subnet_test_id, user_test_id};
        use ic_types::ingress::{IngressState, IngressStatus, WasmResult};

        let user_id = user_test_id(1);
        let canister_id = canister_test_id(1);
        let time = mock_time();
        let mut state = ReplicatedState::new(subnet_test_id(1), SubnetType::Application);
        state.metadata.certification_version = CURRENT_CERTIFICATION_VERSION;
        state.set_ingress_status(
            message_test_id(1),
            IngressStatus::Unknown,
            NumBytes::from(u64::MAX),
        );
        state.set_ingress_status(
            message_test_id(2),
            IngressStatus::Known {
                receiver: canister_id.get(),
                user_id,
                time,
                state: IngressState::Processing,
            },
            NumBytes::from(u64::MAX),
        );
        state.set_ingress_status(
            message_test_id(3),
            IngressStatus::Known {
                receiver: canister_id.get(),
                user_id,
                time,
                state: IngressState::Received,
            },
            NumBytes::from(u64::MAX),
        );
        state.set_ingress_status(
            message_test_id(4),
            IngressStatus::Known {
                receiver: canister_id.get(),
                user_id,
                time,
                state: IngressState::Failed(UserError::new(
                    ErrorCode::SubnetOversubscribed,
                    "subnet oversubscribed",
                )),
            },
            NumBytes::from(u64::MAX),
        );
        state.set_ingress_status(
            message_test_id(5),
            IngressStatus::Known {
                receiver: canister_id.get(),
                user_id,
                time,
                state: IngressState::Completed(WasmResult::Reply(b"reply".to_vec())),
            },
            NumBytes::from(u64::MAX),
        );
        state.set_ingress_status(
            message_test_id(6),
            IngressStatus::Known {
                receiver: canister_id.get(),
                user_id,
                time,
                state: IngressState::Completed(WasmResult::Reject("reject".to_string())),
            },
            NumBytes::from(u64::MAX),
        );

        let pattern = Pattern::match_only("request_status", Pattern::all());
        let visitor = SubtreeVisitor::new(&pattern, TracingVisitor::new(NoopVisitor));
        assert_eq!(
            vec![
                E::StartSubtree,
                edge("request_status"),
                E::StartSubtree,
                //
                edge(message_test_id(1)),
                E::StartSubtree,
                edge("status"),
                E::VisitBlob(b"unknown".to_vec()),
                E::EndSubtree,
                //
                edge(message_test_id(2)),
                E::StartSubtree,
                edge("status"),
                E::VisitBlob(b"processing".to_vec()),
                E::EndSubtree,
                //
                edge(message_test_id(3)),
                E::StartSubtree,
                edge("status"),
                E::VisitBlob(b"received".to_vec()),
                E::EndSubtree,
                //
                edge(message_test_id(4)),
                E::StartSubtree,
                edge("error_code"),
                E::VisitBlob(b"IC0101".to_vec()),
                edge("reject_code"),
                leb_num(1),
                edge("reject_message"),
                E::VisitBlob(b"subnet oversubscribed".to_vec()),
                edge("status"),
                E::VisitBlob(b"rejected".to_vec()),
                E::EndSubtree,
                //
                edge(message_test_id(5)),
                E::StartSubtree,
                edge("reply"),
                E::VisitBlob(b"reply".to_vec()),
                edge("status"),
                E::VisitBlob(b"replied".to_vec()),
                E::EndSubtree,
                //
                edge(message_test_id(6)),
                E::StartSubtree,
                edge("error_code"),
                E::VisitBlob(b"IC0516".to_vec()),
                edge("reject_code"),
                leb_num(4),
                edge("reject_message"),
                E::VisitBlob(b"reject".to_vec()),
                edge("status"),
                E::VisitBlob(b"rejected".to_vec()),
                E::EndSubtree,
                //
                E::EndSubtree,
                E::EndSubtree,
            ],
            traverse(&state, visitor).0
        );
    }

    #[test]
    fn test_traverse_time() {
        let mut state = ReplicatedState::new(subnet_test_id(1), SubnetType::Application);
        state.metadata.certification_version = CURRENT_CERTIFICATION_VERSION;
        state.metadata.batch_time += Duration::new(1, 123456789);

        let visitor = TracingVisitor::new(NoopVisitor);
        assert_eq!(
            vec![
                E::StartSubtree,
                edge("canister"),
                E::StartSubtree,
                E::EndSubtree, // canisters
                edge("metadata"),
                E::VisitBlob(encode_metadata(SystemMetadata {
                    id_counter: None,
                    prev_state_hash: None
                })),
                edge("request_status"),
                E::StartSubtree,
                E::EndSubtree, // request_status
                edge("streams"),
                E::StartSubtree,
                E::EndSubtree, // streams
                edge("subnet"),
                E::StartSubtree,
                E::EndSubtree, // subnets
                edge("time"),
                leb_num(1123456789),
                E::EndSubtree, // global
            ],
            traverse(&state, visitor).0
        );
    }

    #[test]
    fn test_traverse_subnet() {
        let mut state = ReplicatedState::new(subnet_test_id(1), SubnetType::Application);

        state.metadata.network_topology.subnets = btreemap! {
            subnet_test_id(0) => SubnetTopology {
                public_key: vec![1, 2, 3, 4],
                nodes: btreemap!{},
                subnet_type: SubnetType::Application,
                subnet_features: SubnetFeatures::default(),
                ecdsa_keys_held: BTreeSet::new(),
            },
            subnet_test_id(1) => SubnetTopology {
                public_key: vec![5, 6, 7, 8],
                nodes: btreemap!{},
                subnet_type: SubnetType::Application,
                subnet_features: SubnetFeatures::default(),
                ecdsa_keys_held: BTreeSet::new(),
            }
        };
        fn id_range(from: u64, to: u64) -> CanisterIdRange {
            CanisterIdRange {
                start: CanisterId::from_u64(from),
                end: CanisterId::from_u64(to),
            }
        }
        state.metadata.network_topology.routing_table = Arc::new(
            RoutingTable::try_from(btreemap! {
                id_range(0, 10) => subnet_test_id(0),
                id_range(11, 20) => subnet_test_id(1),
                id_range(21, 30) => subnet_test_id(0),
            })
            .unwrap(),
        );

        let visitor = TracingVisitor::new(NoopVisitor);
        state.metadata.certification_version = CertificationVersion::V2;
        assert_eq!(
            vec![
                E::StartSubtree,
                edge("canister"),
                E::StartSubtree,
                E::EndSubtree, // canisters
                edge("metadata"),
                E::VisitBlob(encode_metadata(SystemMetadata {
                    id_counter: Some(0),
                    prev_state_hash: None
                })),
                edge("request_status"),
                E::StartSubtree,
                E::EndSubtree, // request_status
                edge("streams"),
                E::StartSubtree,
                E::EndSubtree, // streams
                edge("subnet"),
                E::StartSubtree,
                E::EnterEdge(subnet_test_id(0).get().into_vec()),
                E::StartSubtree,
                edge("public_key"),
                E::VisitBlob(vec![1, 2, 3, 4]),
                E::EndSubtree, // subnet
                E::EnterEdge(subnet_test_id(1).get().into_vec()),
                E::StartSubtree,
                edge("public_key"),
                E::VisitBlob(vec![5, 6, 7, 8]),
                E::EndSubtree, // subnet
                E::EndSubtree, // subnets
                edge("time"),
                leb_num(0),
                E::EndSubtree, // global
            ],
            traverse(&state, visitor).0
        );

        let pattern = Pattern::match_only("subnet", Pattern::all());
        let visitor = SubtreeVisitor::new(&pattern, TracingVisitor::new(NoopVisitor));
        state.metadata.certification_version = CURRENT_CERTIFICATION_VERSION;
        assert_eq!(
            vec![
                E::StartSubtree,
                edge("subnet"),
                E::StartSubtree,
                E::EnterEdge(subnet_test_id(0).get().into_vec()),
                E::StartSubtree,
                edge("canister_ranges"),
                //D9 D9F7                          # tag(55799)
                //   82                            # array(2)
                //      82                         # array(2)
                //         4A                      # bytes(10)
                //            00000000000000000101 # "\x00\x00\x00\x00\x00\x00\x00\x00\x01\x01"
                //         4A                      # bytes(10)
                //            000000000000000A0101 # "\x00\x00\x00\x00\x00\x00\x00\n\x01\x01"
                //      82                         # array(2)
                //         4A                      # bytes(10)
                //            00000000000000150101 # "\x00\x00\x00\x00\x00\x00\x00\x15\x01\x01"
                //         4A                      # bytes(10)
                //            000000000000001E0101 # "\x00\x00\x00\x00\x00\x00\x00\x1E\x01\x01"
                E::VisitBlob(hex::decode("d9d9f782824a000000000000000001014a000000000000000a0101824a000000000000001501014a000000000000001e0101").unwrap()),
                edge("public_key"),
                E::VisitBlob(vec![1, 2, 3, 4]),
                E::EndSubtree, // subnet
                E::EnterEdge(subnet_test_id(1).get().into_vec()),
                E::StartSubtree,
                edge("canister_ranges"),
                // D9 D9F7                          # tag(55799)
                //    81                            # array(1)
                //       82                         # array(2)
                //          4A                      # bytes(10)
                //             000000000000000B0101 # "\x00\x00\x00\x00\x00\x00\x00\v\x01\x01"
                //          4A                      # bytes(10)
                //             00000000000000140101 # "\x00\x00\x00\x00\x00\x00\x00\x14\x01\x01"
                E::VisitBlob(hex::decode("d9d9f781824a000000000000000b01014a00000000000000140101").unwrap()),
                edge("public_key"),
                E::VisitBlob(vec![5, 6, 7, 8]),
                E::EndSubtree, // subnet
                E::EndSubtree, // subnets
                E::EndSubtree, // global
            ],
            traverse(&state, visitor).0
        );
    }
}
