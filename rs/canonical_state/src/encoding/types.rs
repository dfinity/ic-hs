//! Canonical types for encoding canonical state tree leaves.
//!
//! These structs mirror the ones defined in the `ic-types` crate, with the
//! intent of providing a stable, efficient representation when serialized as
//! CBOR. `From` and `TryFrom` implementations allow converting back and forth
//! between these canonical types and the `ic-types` ones.
//!
//! Enums are encoded as structs with optional fields that are not encoded when
//! `None`. C-like enums are represented as the corresponding unsigned value.
//! Newtypes, such as various IDs are replaced by the wrapped type.
//! `CanisterIds` are represented as byte vectors.

use crate::CertificationVersion;
use ic_error_types::TryFromError;
use ic_protobuf::proxy::ProxyDecodeError;
use ic_types::xnet::StreamIndex;
use serde::{Deserialize, Serialize};
use std::{
    collections::VecDeque,
    convert::{From, Into, TryFrom, TryInto},
    sync::Arc,
};

pub(crate) type Bytes = Vec<u8>;

/// Canonical representation of `ic_types::xnet::StreamHeader`.
#[derive(Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct StreamHeader {
    pub begin: u64,
    pub end: u64,
    pub signals_end: u64,
    /// Delta encoded reject signals: the last signal is encoded as the delta
    /// between `signals_end` and the stream index of the rejected message; all
    /// other signals are encoded as the delta between the next stream index and
    /// the current one.
    ///
    /// Note that `signals_end` is NOT part of the reject signals.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub reject_signal_deltas: Vec<u64>,
}

/// Canonical representation of `ic_types::messages::RequestOrResponse`.
#[derive(Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RequestOrResponse {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request: Option<Request>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub response: Option<Response>,
}

/// Canonical representation of `ic_types::messages::Request`.
#[derive(Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Request {
    #[serde(with = "serde_bytes")]
    pub receiver: Bytes,
    #[serde(with = "serde_bytes")]
    pub sender: Bytes,
    pub sender_reply_callback: u64,
    pub payment: Funds,
    pub method_name: String,
    #[serde(with = "serde_bytes")]
    pub method_payload: Bytes,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cycles_payment: Option<Cycles>,
}

/// Canonical representation of `ic_types::messages::Response`.
#[derive(Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Response {
    #[serde(with = "serde_bytes")]
    pub originator: Bytes,
    #[serde(with = "serde_bytes")]
    pub respondent: Bytes,
    pub originator_reply_callback: u64,
    pub refund: Funds,
    pub response_payload: Payload,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cycles_refund: Option<Cycles>,
}

/// Canonical representation of `ic_types::funds::Cycles`.
#[derive(Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Cycles {
    pub low: u64,
    // TODO(EXC-337) `Skip` used for maintaining the serialisation backward compatible.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub high: Option<u64>,
}

/// Canonical representation of `ic_types::funds::Funds`.
#[derive(Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Funds {
    pub cycles: Cycles,
    #[serde(skip_serializing_if = "is_zero", default)]
    pub icp: u64,
}

pub fn is_zero(v: &u64) -> bool {
    *v == 0
}

/// Canonical representation of `ic_types::messages::Payload`.
#[derive(Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Payload {
    #[serde(with = "serde_bytes", skip_serializing_if = "Option::is_none", default)]
    pub data: Option<Bytes>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reject: Option<RejectContext>,
}

/// Canonical representation of `ic_types::messages::RejectContext`.
#[derive(Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RejectContext {
    pub code: u8,
    pub message: String,
}

/// Canonical representation of state metadata leaf.
#[derive(Debug, Serialize)]
pub struct SystemMetadata {
    /// The counter used to allocate canister ids.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id_counter: Option<u64>,
    /// Hash bytes of the previous (partial) canonical state.
    pub prev_state_hash: Option<Vec<u8>>,
}

impl From<(&ic_types::xnet::StreamHeader, CertificationVersion)> for StreamHeader {
    fn from(
        (header, certification_version): (&ic_types::xnet::StreamHeader, CertificationVersion),
    ) -> Self {
        // Replicas with certification version < 9 do not produce reject signals. This
        // includes replicas with certification version 8, but they may "inherit" reject
        // signals from a replica with certification version 9 after a downgrade.
        assert!(
            header.reject_signals.is_empty() || certification_version >= CertificationVersion::V8,
            "Replicas with certification version < 9 should not be producing reject signals"
        );

        let mut next_index = header.signals_end;
        let mut reject_signal_deltas = vec![0; header.reject_signals.len()];
        for (i, stream_index) in header.reject_signals.iter().enumerate().rev() {
            assert!(next_index > *stream_index);
            reject_signal_deltas[i] = next_index.get() - stream_index.get();
            next_index = *stream_index;
        }

        Self {
            begin: header.begin.get(),
            end: header.end.get(),
            signals_end: header.signals_end.get(),
            reject_signal_deltas,
        }
    }
}

impl TryFrom<StreamHeader> for ic_types::xnet::StreamHeader {
    type Error = ProxyDecodeError;
    fn try_from(header: StreamHeader) -> Result<Self, Self::Error> {
        let mut reject_signals = VecDeque::with_capacity(header.reject_signal_deltas.len());
        let mut stream_index = StreamIndex::new(header.signals_end);
        for delta in header.reject_signal_deltas.iter().rev() {
            if stream_index < StreamIndex::new(*delta) {
                // Reject signal deltas are invalid.
                return Err(ProxyDecodeError::Other(format!(
                    "StreamHeader: reject signals are invalid, got `signals_end` {:?}, `reject_signal_deltas` {:?}",
                    header.signals_end,
                    header.reject_signal_deltas,
                )));
            }
            stream_index -= StreamIndex::new(*delta);
            reject_signals.push_front(stream_index);
        }

        Ok(Self {
            begin: header.begin.into(),
            end: header.end.into(),
            signals_end: header.signals_end.into(),
            reject_signals,
        })
    }
}

impl From<(&ic_types::messages::RequestOrResponse, CertificationVersion)> for RequestOrResponse {
    fn from(
        (message, certification_version): (
            &ic_types::messages::RequestOrResponse,
            CertificationVersion,
        ),
    ) -> Self {
        use ic_types::messages::RequestOrResponse::*;
        match message {
            Request(request) => Self {
                request: Some((request.as_ref(), certification_version).into()),
                response: None,
            },
            Response(response) => Self {
                request: None,
                response: Some((response.as_ref(), certification_version).into()),
            },
        }
    }
}

impl TryFrom<RequestOrResponse> for ic_types::messages::RequestOrResponse {
    type Error = ProxyDecodeError;

    fn try_from(message: RequestOrResponse) -> Result<Self, Self::Error> {
        match message {
            RequestOrResponse {
                request: Some(request),
                response: None,
            } => Ok(Self::Request(Arc::new(request.try_into()?))),
            RequestOrResponse {
                request: None,
                response: Some(response),
            } => Ok(Self::Response(Arc::new(response.try_into()?))),
            other => Err(ProxyDecodeError::Other(format!(
                "RequestOrResponse: expected exactly one of `request` or `response` to be `Some(_)`, got `{:?}`",
                other
            )))
        }
    }
}

impl From<(&ic_types::messages::Request, CertificationVersion)> for Request {
    fn from(
        (request, certification_version): (&ic_types::messages::Request, CertificationVersion),
    ) -> Self {
        let funds = Funds {
            cycles: (&request.payment, certification_version).into(),
            icp: 0,
        };
        Self {
            receiver: request.receiver.get().to_vec(),
            sender: request.sender.get().to_vec(),
            sender_reply_callback: request.sender_reply_callback.get(),
            payment: funds,
            method_name: request.method_name.clone(),
            method_payload: request.method_payload.clone(),
            cycles_payment: None,
        }
    }
}

impl TryFrom<Request> for ic_types::messages::Request {
    type Error = ProxyDecodeError;

    fn try_from(request: Request) -> Result<Self, Self::Error> {
        let payment = match request.cycles_payment {
            Some(cycles) => cycles,
            None => request.payment.cycles,
        }
        .try_into()?;

        Ok(Self {
            receiver: ic_types::CanisterId::new(request.receiver.as_slice().try_into()?)?,
            sender: ic_types::CanisterId::new(request.sender.as_slice().try_into()?)?,
            sender_reply_callback: request.sender_reply_callback.into(),
            payment,
            method_name: request.method_name,
            method_payload: request.method_payload,
            callback: None,
        })
    }
}

impl From<(&ic_types::messages::Response, CertificationVersion)> for Response {
    fn from(
        (response, certification_version): (&ic_types::messages::Response, CertificationVersion),
    ) -> Self {
        let funds = Funds {
            cycles: (&response.refund, certification_version).into(),
            icp: 0,
        };
        Self {
            originator: response.originator.get().to_vec(),
            respondent: response.respondent.get().to_vec(),
            originator_reply_callback: response.originator_reply_callback.get(),
            refund: funds,
            response_payload: (&response.response_payload, certification_version).into(),
            cycles_refund: None,
        }
    }
}

impl TryFrom<Response> for ic_types::messages::Response {
    type Error = ProxyDecodeError;

    fn try_from(response: Response) -> Result<Self, Self::Error> {
        let refund = match response.cycles_refund {
            Some(cycles) => cycles,
            None => response.refund.cycles,
        }
        .try_into()?;

        Ok(Self {
            originator: ic_types::CanisterId::new(response.originator.as_slice().try_into()?)?,
            respondent: ic_types::CanisterId::new(response.respondent.as_slice().try_into()?)?,
            originator_reply_callback: response.originator_reply_callback.into(),
            refund,
            response_payload: response.response_payload.try_into()?,
        })
    }
}

impl From<(&ic_types::funds::Cycles, CertificationVersion)> for Cycles {
    fn from(
        (cycles, _certification_version): (&ic_types::funds::Cycles, CertificationVersion),
    ) -> Self {
        let (high, low) = cycles.into_parts();
        Self {
            low,
            // For backward compatibility, set to `None` when the upper 8 bytes are missing.
            high: match high {
                0 => None,
                _ => Some(high),
            },
        }
    }
}

impl TryFrom<Cycles> for ic_types::funds::Cycles {
    type Error = ProxyDecodeError;

    fn try_from(cycles: Cycles) -> Result<Self, Self::Error> {
        match cycles.high {
            None => Ok(Self::from(cycles.low)),
            Some(high) => Ok(Self::from_parts(high, cycles.low)),
        }
    }
}

impl From<(&ic_types::funds::Funds, CertificationVersion)> for Funds {
    fn from(
        (funds, certification_version): (&ic_types::funds::Funds, CertificationVersion),
    ) -> Self {
        Self {
            cycles: (&funds.cycles(), certification_version).into(),
            icp: 0,
        }
    }
}

impl TryFrom<Funds> for ic_types::funds::Funds {
    type Error = ProxyDecodeError;

    fn try_from(funds: Funds) -> Result<Self, Self::Error> {
        Ok(Self::new(funds.cycles.try_into()?))
    }
}

impl From<(&ic_types::messages::Payload, CertificationVersion)> for Payload {
    fn from(
        (payload, certification_version): (&ic_types::messages::Payload, CertificationVersion),
    ) -> Self {
        use ic_types::messages::Payload::*;
        match payload {
            Data(data) => Self {
                data: Some(data.clone()),
                reject: None,
            },
            Reject(reject) => Self {
                data: None,
                reject: Some((reject, certification_version).into()),
            },
        }
    }
}

impl TryFrom<Payload> for ic_types::messages::Payload {
    type Error = ProxyDecodeError;

    fn try_from(payload: Payload) -> Result<Self, Self::Error> {
        match payload {
            Payload {
                data: Some(data),
                reject: None,
            } => Ok(Self::Data(data)),
            Payload {
                data: None,
                reject: Some(reject),
            } => Ok(Self::Reject(reject.try_into()?)),
            other => Err(ProxyDecodeError::Other(format!(
                "Payload: expected exactly one of `data` or `reject` to be `Some(_)`, got `{:?}`",
                other
            ))),
        }
    }
}

impl From<(&ic_types::messages::RejectContext, CertificationVersion)> for RejectContext {
    fn from(
        (context, _certification_version): (
            &ic_types::messages::RejectContext,
            CertificationVersion,
        ),
    ) -> Self {
        Self {
            code: context.code as u8,
            message: context.message.clone(),
        }
    }
}

impl TryFrom<RejectContext> for ic_types::messages::RejectContext {
    type Error = ProxyDecodeError;

    fn try_from(context: RejectContext) -> Result<Self, Self::Error> {
        Ok(Self {
            code: (context.code as u64).try_into().map_err(|err| match err {
                TryFromError::ValueOutOfRange(code) => ProxyDecodeError::ValueOutOfRange {
                    typ: "RejectContext",
                    err: code.to_string(),
                },
            })?,
            message: context.message,
        })
    }
}

impl
    From<(
        &ic_replicated_state::metadata_state::SystemMetadata,
        CertificationVersion,
    )> for SystemMetadata
{
    fn from(
        (metadata, certification_version): (
            &ic_replicated_state::metadata_state::SystemMetadata,
            CertificationVersion,
        ),
    ) -> Self {
        Self {
            id_counter: if certification_version <= CertificationVersion::V9 {
                Some(0)
            } else {
                None
            },
            prev_state_hash: metadata
                .prev_state_hash
                .as_ref()
                .map(|h| h.get_ref().0.clone()),
        }
    }
}
