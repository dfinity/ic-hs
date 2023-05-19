//! Collection of canonical tree leaf backwards-compatibility tests.
//!
//! Any breakage of these tests likely means that the canonical state
//! representation and/or the XNet messaging protocol have changed and may no
//! longer be backwards-compatible.
//!
//! Such changes must be rolled out in stages, in order to maintain backwards
//! (and ideally forwards) compatibility with one or more preceeding
//! protocol versions.

use crate::{
    all_supported_versions, encoding::*, CertificationVersion, CURRENT_CERTIFICATION_VERSION,
};
use assert_matches::assert_matches;
use ic_error_types::RejectCode;
use ic_registry_subnet_type::SubnetType;
use ic_replicated_state::metadata_state::SystemMetadata;
use ic_test_utilities::types::{
    ids::{canister_test_id, subnet_test_id},
    messages::{RequestBuilder, ResponseBuilder},
};
use ic_types::{
    crypto::CryptoHash,
    messages::{CallbackId, Payload, RejectContext, Request, RequestOrResponse, Response},
    xnet::StreamHeader,
    CryptoHashOfPartialState, Cycles, Funds,
};
use serde_cbor::value::Value;
use std::collections::{BTreeMap, VecDeque};

//
// Tests for exact binary encoding
//

/// Canonical CBOR encoding of:
///
/// ```no_run
/// StreamHeader {
///     begin: 23.into(),
///     end: 25.into(),
///     signals_end: 256.into(),
///     reject_signals: VecDeque::new(),
/// }
/// ```
///
/// Expected:
///
/// ```text
/// A3         # map(3)
///    00      # field_index(StreamHeader::begin)
///    17      # unsigned(23)
///    01      # field_index(StreamHeader::end)
///    18 19   # unsigned(25)
///    02      # field_index(StreamHeader::signals_end)
///    19 0100 # unsigned(256)
/// ```
#[test]
fn canonical_encoding_stream_header() {
    for certification_version in all_supported_versions() {
        let header = StreamHeader {
            begin: 23.into(),
            end: 25.into(),
            signals_end: 256.into(),
            reject_signals: VecDeque::new(),
        };

        assert_eq!(
            "A3 00 17 01 18 19 02 19 01 00",
            as_hex(&encode_stream_header(&header, certification_version,))
        );
    }
}

/// Canonical CBOR encoding (with certification versions 8 and up) of:
///
/// ```no_run
/// StreamHeader {
///     begin: 23.into(),
///     end: 25.into(),
///     signals_end: 256.into(),
///     reject_signals: vec![249.into(), 250.into(), 252.into()].into(),
/// }
/// ```
///
/// Expected:
///
/// ```text
/// A4         # map(4)
///    00      # field_index(StreamHeader::begin)
///    17      # unsigned(23)
///    01      # field_index(StreamHeader::end)
///    18 19   # unsigned(25)
///    02      # field_index(StreamHeader::signals_end)
///    19 0100 # unsigned(256)
///    03      # field_index(StreamHeader::reject_signals)
///    83      # array(3)
///       01   # unsigned(1)
///       02   # unsigned(2)
///       04   # unsigned(4)
/// ```
#[test]
fn canonical_encoding_stream_header_v8_plus() {
    for certification_version in all_supported_versions().filter(|v| v >= &CertificationVersion::V8)
    {
        let header = StreamHeader {
            begin: 23.into(),
            end: 25.into(),
            signals_end: 256.into(),
            reject_signals: vec![249.into(), 250.into(), 252.into()].into(),
        };

        assert_eq!(
            "A4 00 17 01 18 19 02 19 01 00 03 83 01 02 04",
            as_hex(&encode_stream_header(&header, certification_version))
        );
    }
}

/// Canonical CBOR encoding of:
///
/// ```no_run
/// RequestOrResponse::Request(
///     Request {
///         receiver: canister_test_id(1),
///         sender: canister_test_id(2),
///         sender_reply_callback: CallbackId::from(3),
///         payment: Cycles::new(3),
///         method_name: "test".to_string(),
///         method_payload: vec![6],
///     }
/// )
/// ```
///
/// Expected:
///
/// ```text
/// A1                            # map(1)
///    00                         # field_index(RequestOrResponse::request)
///    A6                         # map(6)
///       00                      # field_index(Request::receiver)
///       4A                      # bytes(10)
///          00000000000000010101 # "\x00\x00\x00\x00\x00\x00\x00\x01\x01\x01"
///       01                      # field_index(Request::sender)
///       4A                      # bytes(10)
///          00000000000000020101 # "\x00\x00\x00\x00\x00\x00\x00\x02\x01\x01"
///       02                      # field_index(Request::sender_reply_callback)
///       03                      # unsigned(3)
///       03                      # field_index(Request::payment)
///       A1                      # map(1)
///          00                   # field_index(Funds::cycles)
///          A1                   # map(1)
///             00                # field_index(Cycles::raw)
///             04                # unsigned(4)
///       04                      # field_index(Request::method_name)
///       64                      # text(4)
///          74657374             # "test"
///       05                      # field_index(Request::method_payload)
///       41                      # bytes(1)
///          06                   # "\x06"
/// Used http://cbor.me/ for printing the human friendly output.
/// ```
#[test]
fn canonical_encoding_request() {
    let request: RequestOrResponse = RequestBuilder::new()
        .receiver(canister_test_id(1))
        .sender(canister_test_id(2))
        .sender_reply_callback(CallbackId::from(3))
        .payment(Cycles::new(4))
        .method_name("test".to_string())
        .method_payload(vec![6])
        .build()
        .into();

    assert_eq!(
        "A1 00 A6 00 4A 00 00 00 00 00 00 00 01 01 01 01 4A 00 00 00 00 00 00 00 02 01 01 02 03 03 A1 00 A1 00 04 04 64 74 65 73 74 05 41 06",
        as_hex(&encode_message(&request, CURRENT_CERTIFICATION_VERSION))
    );
}

/// Canonical CBOR encoding of:
///
/// ```no_run
/// RequestOrResponse::Request(
///     Request {
///         receiver: canister_test_id(1),
///         sender: canister_test_id(2),
///         sender_reply_callback: CallbackId::from(3),
///         payment: Funds::new(Cycles::new(123456789012345678901234567890)),
///         method_name: "test".to_string(),
///         method_payload: vec![6],
///     }
/// )
/// ```
///
/// Expected:
///
/// ```text
/// A1                            # map(1)
///    00                         # field_index(RequestOrResponse::request)
///    A6                         # map(6)
///       00                      # field_index(Request::receiver)
///       4A                      # bytes(10)
///          00000000000000010101 # "\x00\x00\x00\x00\x00\x00\x00\x01\x01\x01"
///       01                      # field_index(Request::sender)
///       4A                      # bytes(10)
///          00000000000000020101 # "\x00\x00\x00\x00\x00\x00\x00\x02\x01\x01"
///       02                      # field_index(Request::sender_reply_callback)
///       03                      # unsigned(3)
///       03                      # field_index(Request::payment)
///       A1                      # map(1)
///          00                   # field_index(Funds::cycles)
///          A2                   # map(2)
///             00                # field_index(Cycles::raw)
///             1B C373E0EE4E3F0AD2  # unsigned(14083847773837265618)
///             01                # field_index(Cycles::upper_raw)
///             1B 000000018EE09FF6  # unsigned(61672207766)
///       04                      # field_index(Request::method_name)
///       64                      # text(4)
///          74657374             # "test"
///       05                      # field_index(Request::method_payload)
///       41                      # bytes(1)
///          06                   # "\x06"
/// Used http://cbor.me/ for printing the human friendly output.
/// ```
#[test]
fn canonical_encoding_request_u128() {
    let request: RequestOrResponse = RequestBuilder::new()
        .receiver(canister_test_id(1))
        .sender(canister_test_id(2))
        .sender_reply_callback(CallbackId::from(3))
        .payment(Cycles::new(123456789012345678901234567890))
        .method_name("test".to_string())
        .method_payload(vec![6])
        .build()
        .into();

    assert_eq!(
        "A1 00 A6 00 4A 00 00 00 00 00 00 00 01 01 01 01 4A 00 00 00 00 00 00 00 02 01 01 02 03 03 A1 00 A2 00 1B C3 73 E0 EE 4E 3F 0A D2 01 1B 00 00 00 01 8E E9 0F F6 04 64 74 65 73 74 05 41 06",
        as_hex(&encode_message(&request, CURRENT_CERTIFICATION_VERSION))
    );
}

/// Canonical CBOR encoding of:
///
/// ```no_run
/// RequestOrResponse::Response(
///     Response {
///         originator: canister_test_id(5),
///         respondent: canister_test_id(4),
///         originator_reply_callback: CallbackId::from(3),
///         refund: Cycles::new(2),
///         response_payload: Payload::Data(vec![1]),
///     }
/// )
/// ```
///
/// Expected:
///
/// ```text
/// A1                            # map(1)
///    01                         # field_index(RequestOrResponse::response)
///    A5                         # map(5)
///       00                      # field_index(Response::originator)
///       4A                      # bytes(10)
///          00000000000000050101 # "\x00\x00\x00\x00\x00\x00\x00\x06\x01\x01"
///       01                      # field_index(Response::respondent)
///       4A                      # bytes(10)
///          00000000000000040101 # "\x00\x00\x00\x00\x00\x00\x00\x05\x01\x01"
///       02                      # field_index(Response::originator_reply_callback)
///       03                      # unsigned(3)
///       03                      # field_index(Response::refund)
///       A1                      # map(1)
///          00                   # field_index(Funds::cycles)
///          A1                   # map(1)
///             00                # field_index(Cycles::raw)
///             02                # unsigned(2)
///       04                      # field_index(Response::response_payload)
///       A1                      # map(1)
///          00                   # field_index(Payload::data)
///          41                   # bytes(1)
///             01                # "\x01"
/// Used http://cbor.me/ for printing the human friendly output.
/// ```
#[test]
fn canonical_encoding_response() {
    let response: RequestOrResponse = ResponseBuilder::new()
        .originator(canister_test_id(5))
        .respondent(canister_test_id(4))
        .originator_reply_callback(CallbackId::from(3))
        .refund(Cycles::new(2))
        .response_payload(Payload::Data(vec![1]))
        .build()
        .into();

    assert_eq!(
        "A1 01 A5 00 4A 00 00 00 00 00 00 00 05 01 01 01 4A 00 00 00 00 00 00 00 04 01 01 02 03 03 A1 00 A1 00 02 04 A1 00 41 01",
        as_hex(&encode_message(&response, CURRENT_CERTIFICATION_VERSION))
    );
}

///
/// Canonical CBOR encoding of:
///
/// ```no_run
/// RequestOrResponse::Response(
///     Response {
///         originator: canister_test_id(5),
///         respondent: canister_test_id(4),
///         originator_reply_callback: CallbackId::from(3),
///         refund: Funds::new(Cycles::new(123456789012345678901234567890)),
///         response_payload: Payload::Data(vec![1]),
///     }
/// )
/// ```
///
/// Expected:
///
/// ```text
/// A1                            # map(1)
///    01                         # field_index(RequestOrResponse::response)
///    A5                         # map(5)
///       00                      # field_index(Response::originator)
///       4A                      # bytes(10)
///          00000000000000050101 # "\x00\x00\x00\x00\x00\x00\x00\x06\x01\x01"
///       01                      # field_index(Response::respondent)
///       4A                      # bytes(10)
///          00000000000000040101 # "\x00\x00\x00\x00\x00\x00\x00\x05\x01\x01"
///       02                      # field_index(Response::originator_reply_callback)
///       03                      # unsigned(3)
///       03                      # field_index(Response::refund)
///       A1                      # map(1)
///          00                   # field_index(Funds::cycles)
///          A2                   # map(2)
///             00                # field_index(Cycles::raw)
///             1B C373E0EE4E3F0AD2  # unsigned(14083847773837265618)
///             01                # field_index(Cycles::upper_raw)
///             1B 000000018EE09FF6  # unsigned(61672207766)
///       04                      # field_index(Response::response_payload)
///       A1                      # map(1)
///          00                   # field_index(Payload::data)
///          41                   # bytes(1)
///             01                # "\x01"
/// Used http://cbor.me/ for printing the human friendly output.
/// ```
#[test]
fn canonical_encoding_response_u128() {
    let response: RequestOrResponse = ResponseBuilder::new()
        .originator(canister_test_id(5))
        .respondent(canister_test_id(4))
        .originator_reply_callback(CallbackId::from(3))
        .refund(Cycles::new(123456789012345678901234567890))
        .response_payload(Payload::Data(vec![1]))
        .build()
        .into();

    assert_eq!(
        "A1 01 A5 00 4A 00 00 00 00 00 00 00 05 01 01 01 4A 00 00 00 00 00 00 00 04 01 01 02 03 03 A1 00 A2 00 1B C3 73 E0 EE 4E 3F 0A D2 01 1B 00 00 00 01 8E E9 0F F6 04 A1 00 41 01",
        as_hex(&encode_message(&response, CURRENT_CERTIFICATION_VERSION))
    );
}

/// Canonical CBOR encoding of:
///
/// ```no_run
/// RequestOrResponse::Response(
///     Response {
///         originator: canister_test_id(6),
///         respondent: canister_test_id(5),
///         originator_reply_callback: CallbackId::from(4),
///         refund: Cycles::new(3),
///         response_payload: Payload::Reject(RejectContext {
///             code: RejectCode::SysFatal,
///             message: "Oops".into(),
///         }),
///     }
/// )
/// ```
///
/// Expected:
///
/// ```text
/// A1                            # map(1)
///    01                         # field_index(RequestOrResponse::response)
///    A5                         # map(5)
///       00                      # field_index(Response::originator)
///       4A                      # bytes(10)
///          00000000000000060101 # "\x00\x00\x00\x00\x00\x00\x00\x06\x01\x01"
///       01                      # field_index(Response::respondent)
///       4A                      # bytes(10)
///          00000000000000050101 # "\x00\x00\x00\x00\x00\x00\x00\x05\x01\x01"
///       02                      # field_index(Response::originator_reply_callback)
///       04                      # unsigned(4)
///       03                      # field_index(Response::refund)
///       A1                      # map(1)
///          00                   # field_index(Funds::cycles)
///          A1                   # map(1)
///             00                # field_index(Cycles::raw)
///             03                # unsigned(3)
///       04                      # field_index(Response::response_payload)
///       A1                      # map(1)
///          01                   # field_index(Payload::reject)
///          A2                   # map(2)
///             00                # field_index(RejectContext::code)
///             01                # unsigned(1)
///             01                # field_index(RejectContext::message)
///             64                # text(4)
///                4F6F7073       # "Oops"
/// ```
#[test]
fn canonical_encoding_reject_response() {
    let reject_response: RequestOrResponse = ResponseBuilder::new()
        .originator(canister_test_id(6))
        .respondent(canister_test_id(5))
        .originator_reply_callback(CallbackId::from(4))
        .refund(Cycles::new(3))
        .response_payload(Payload::Reject(RejectContext {
            code: RejectCode::SysFatal,
            message: "Oops".into(),
        }))
        .build()
        .into();

    assert_eq!(
        "A1 01 A5 00 4A 00 00 00 00 00 00 00 06 01 01 01 4A 00 00 00 00 00 00 00 05 01 01 02 04 03 A1 00 A1 00 03 04 A1 01 A2 00 01 01 64 4F 6F 70 73",
        as_hex(&encode_message(&reject_response, CURRENT_CERTIFICATION_VERSION))
    );
}

/// Canonical CBOR encoding of:
///
/// ```no_run
/// SystemMetadata{
///     own_subnet_id: new(subnet_test_id(13)),
///     generated_id_counter, 14,
///     prev_state_hash: Some(CryptoHashOfPartialState::new(CryptoHash(vec![15]))),
///     ..Default::default()
/// }
/// ```
///
/// Expected:
///
/// ```text
/// A2       # map(2)
///    00    # field_index(SystemMetadata::id_counter)
///    00    # unsigned(0)
///    01    # field_index(SystemMetadata::prev_state_hash)
///    81    # array(1)
///       0F # unsigned(15)
/// ```
#[test]
fn canonical_encoding_system_metadata() {
    for certification_version in all_supported_versions() {
        let mut metadata = SystemMetadata::new(subnet_test_id(13), SubnetType::Application);
        metadata.prev_state_hash = Some(CryptoHashOfPartialState::new(CryptoHash(vec![15])));

        if certification_version <= CertificationVersion::V9 {
            // `generated_id_counter` encoded up to and including V9.
            assert_eq!(
                "A2 00 00 01 81 0F",
                as_hex(&encode_metadata(&metadata, certification_version))
            );
        } else {
            // `generated_id_counter` not encoded starting with V10.
            assert_eq!(
                "A1 01 81 0F",
                as_hex(&encode_metadata(&metadata, certification_version))
            );
        }
    }
}

//
// `RequestOrResponse` decoding
//

#[test]
#[should_panic(expected = "expected field index 0 <= i < 2")]
fn invalid_message_extra_field() {
    for certification_version in all_supported_versions() {
        let bytes = types::RequestOrResponse::encode_with_extra_field((
            &request_message(),
            certification_version,
        ))
        .unwrap();

        let _: RequestOrResponse = types::RequestOrResponse::proxy_decode(&bytes).unwrap();
    }
}

#[test]
#[should_panic(
    expected = "RequestOrResponse: expected exactly one of `request` or `response` to be `Some(_)`, got `RequestOrResponse { request: None, response: None }`"
)]
fn invalid_message_empty() {
    for certification_version in all_supported_versions() {
        let bytes = types::RequestOrResponse::encode_without_field(
            (&request_message(), certification_version),
            0,
        )
        .unwrap();

        let _: RequestOrResponse = types::RequestOrResponse::proxy_decode(&bytes).unwrap();
    }
}

//
// `Request` decoding
//

#[test]
fn valid_request() {
    for certification_version in all_supported_versions() {
        let request = request();
        let bytes = types::Request::proxy_encode((&request, certification_version)).unwrap();

        assert_eq!(request, types::Request::proxy_decode(&bytes).unwrap());
    }
}

#[test]
fn invalid_request_extra_field() {
    for certification_version in all_supported_versions() {
        let bytes =
            types::Request::encode_with_extra_field((&request(), certification_version)).unwrap();

        let res: Result<Request, ProxyDecodeError> = types::Request::proxy_decode(&bytes);
        assert_matches!(
            res,
            Err(ProxyDecodeError::CborDecodeError(err))
                if err.to_string().contains("expected field index 0 <= i < 7")
        );
    }
}

#[test]
#[should_panic(expected = "missing field `receiver`")]
fn invalid_request_missing_receiver() {
    for certification_version in all_supported_versions() {
        let bytes =
            types::Request::encode_without_field((&request(), certification_version), 0).unwrap();

        let _: Request = types::Request::proxy_decode(&bytes).unwrap();
    }
}

#[test]
#[should_panic(expected = "missing field `sender`")]
fn invalid_request_missing_sender() {
    for certification_version in all_supported_versions() {
        let bytes =
            types::Request::encode_without_field((&request(), certification_version), 1).unwrap();

        let _: Request = types::Request::proxy_decode(&bytes).unwrap();
    }
}

#[test]
#[should_panic(expected = "missing field `sender_reply_callback`")]
fn invalid_request_missing_sender_reply_callback() {
    for certification_version in all_supported_versions() {
        let bytes =
            types::Request::encode_without_field((&request(), certification_version), 2).unwrap();

        let _: Request = types::Request::proxy_decode(&bytes).unwrap();
    }
}

#[test]
#[should_panic(expected = "missing field `payment`")]
fn invalid_request_missing_payment() {
    for certification_version in all_supported_versions() {
        let bytes =
            types::Request::encode_without_field((&request(), certification_version), 3).unwrap();

        let _: Request = types::Request::proxy_decode(&bytes).unwrap();
    }
}

#[test]
#[should_panic(expected = "missing field `method_name`")]
fn invalid_request_missing_method_name() {
    for certification_version in all_supported_versions() {
        let bytes =
            types::Request::encode_without_field((&request(), certification_version), 4).unwrap();

        let _: Request = types::Request::proxy_decode(&bytes).unwrap();
    }
}

#[test]
#[should_panic(expected = "missing field `method_payload`")]
fn invalid_request_missing_method_payload() {
    for certification_version in all_supported_versions() {
        let bytes =
            types::Request::encode_without_field((&request(), certification_version), 5).unwrap();

        let _: Request = types::Request::proxy_decode(&bytes).unwrap();
    }
}

//
// `Response` decoding
//

#[test]
fn valid_response() {
    for certification_version in all_supported_versions() {
        let response = response();
        let bytes = types::Response::proxy_encode((&response, certification_version)).unwrap();

        assert_eq!(response, types::Response::proxy_decode(&bytes).unwrap());
    }
}

#[test]
fn invalid_response_extra_field() {
    for certification_version in all_supported_versions() {
        let bytes =
            types::Response::encode_with_extra_field((&response(), certification_version)).unwrap();

        let res: Result<Response, ProxyDecodeError> = types::Response::proxy_decode(&bytes);
        assert_matches!(
            res,
            Err(ProxyDecodeError::CborDecodeError(err))
                if err.to_string().contains("expected field index 0 <= i < 6")
        );
    }
}

#[test]
#[should_panic(expected = "missing field `originator`")]
fn invalid_response_missing_originator() {
    for certification_version in all_supported_versions() {
        let bytes =
            types::Response::encode_without_field((&response(), certification_version), 0).unwrap();

        let _: Response = types::Response::proxy_decode(&bytes).unwrap();
    }
}

#[test]
#[should_panic(expected = "missing field `respondent`")]
fn invalid_response_missing_respondent() {
    for certification_version in all_supported_versions() {
        let bytes =
            types::Response::encode_without_field((&response(), certification_version), 1).unwrap();

        let _: Response = types::Response::proxy_decode(&bytes).unwrap();
    }
}

#[test]
#[should_panic(expected = "missing field `originator_reply_callback`")]
fn invalid_response_missing_originator_reply_callback() {
    for certification_version in all_supported_versions() {
        let bytes =
            types::Response::encode_without_field((&response(), certification_version), 2).unwrap();

        let _: Response = types::Response::proxy_decode(&bytes).unwrap();
    }
}

#[test]
#[should_panic(expected = "missing field `refund`")]
fn invalid_response_missing_refund() {
    for certification_version in all_supported_versions() {
        let bytes =
            types::Response::encode_without_field((&response(), certification_version), 3).unwrap();

        let _: Response = types::Response::proxy_decode(&bytes).unwrap();
    }
}

#[test]
#[should_panic(expected = "missing field `response_payload`")]
fn invalid_response_missing_response_payload() {
    for certification_version in all_supported_versions() {
        let bytes =
            types::Response::encode_without_field((&response(), certification_version), 4).unwrap();

        let _: Response = types::Response::proxy_decode(&bytes).unwrap();
    }
}

//
// `Funds` decoding
//

#[test]
fn valid_funds() {
    for certification_version in all_supported_versions() {
        let funds = funds();
        let bytes = types::Funds::proxy_encode((&funds, certification_version)).unwrap();

        assert_eq!(funds, types::Funds::proxy_decode(&bytes).unwrap());
    }
}

#[test]
#[should_panic(expected = "expected field index 0 <= i < 2")]
fn invalid_funds_extra_field() {
    for certification_version in all_supported_versions() {
        let bytes =
            types::Funds::encode_with_extra_field((&funds(), certification_version)).unwrap();

        let _: Funds = types::Funds::proxy_decode(&bytes).unwrap();
    }
}

#[test]
#[should_panic(expected = "missing field `cycles`")]
fn invalid_funds_missing_cycles() {
    for certification_version in all_supported_versions() {
        let bytes =
            types::Funds::encode_without_field((&funds(), certification_version), 0).unwrap();

        let _: Funds = types::Funds::proxy_decode(&bytes).unwrap();
    }
}

//
// `Payload` decoding
//

#[test]
fn valid_data_payload() {
    for certification_version in all_supported_versions() {
        let payload = data_payload();
        let bytes = types::Payload::proxy_encode((&payload, certification_version)).unwrap();

        assert_eq!(payload, types::Payload::proxy_decode(&bytes).unwrap());
    }
}

#[test]
fn valid_reject_payload() {
    for certification_version in all_supported_versions() {
        let payload = reject_payload();
        let bytes = types::Payload::proxy_encode((&payload, certification_version)).unwrap();

        assert_eq!(payload, types::Payload::proxy_decode(&bytes).unwrap());
    }
}

#[test]
#[should_panic(expected = "expected field index 0 <= i < 2")]
fn invalid_payload_extra_field() {
    for certification_version in all_supported_versions() {
        let bytes =
            types::Payload::encode_with_extra_field((&data_payload(), certification_version))
                .unwrap();

        let _: Payload = types::Payload::proxy_decode(&bytes).unwrap();
    }
}

#[test]
#[should_panic(
    expected = "Payload: expected exactly one of `data` or `reject` to be `Some(_)`, got `Payload { data: None, reject: None }`"
)]
fn invalid_payload_empty() {
    for certification_version in all_supported_versions() {
        let bytes =
            types::Payload::encode_without_field((&data_payload(), certification_version), 0)
                .unwrap();

        let _: Payload = types::Payload::proxy_decode(&bytes).unwrap();
    }
}

//
// `RejectContext` decoding
//

#[test]
fn valid_reject_context() {
    for certification_version in all_supported_versions() {
        let context = reject_context();
        let bytes = types::RejectContext::proxy_encode((&context, certification_version)).unwrap();

        assert_eq!(context, types::RejectContext::proxy_decode(&bytes).unwrap());
    }
}

#[test]
#[should_panic(expected = "expected field index 0 <= i < 2")]
fn invalid_reject_context_extra_field() {
    for certification_version in all_supported_versions() {
        let bytes = types::RejectContext::encode_with_extra_field((
            &reject_context(),
            certification_version,
        ))
        .unwrap();

        let _: RejectContext = types::RejectContext::proxy_decode(&bytes).unwrap();
    }
}

#[test]
#[should_panic(expected = "missing field `code`")]
fn invalid_reject_context_missing_code() {
    for certification_version in all_supported_versions() {
        let bytes = types::RejectContext::encode_without_field(
            (&reject_context(), certification_version),
            0,
        )
        .unwrap();

        let _: RejectContext = types::RejectContext::proxy_decode(&bytes).unwrap();
    }
}

#[test]
#[should_panic(expected = "missing field `message`")]
fn invalid_reject_context_missing_message() {
    for certification_version in all_supported_versions() {
        let bytes = types::RejectContext::encode_without_field(
            (&reject_context(), certification_version),
            1,
        )
        .unwrap();

        let _: RejectContext = types::RejectContext::proxy_decode(&bytes).unwrap();
    }
}

/// Converts a blob into a hex representation that can be pasted into http://cbor.me
pub fn as_hex(buf: &[u8]) -> String {
    const DIGITS: &[u8; 16] = b"0123456789ABCDEF";

    if buf.is_empty() {
        return "".into();
    }

    let mut res = String::with_capacity(3 * buf.len());
    for b in buf {
        res.push(DIGITS[*b as usize >> 4] as char);
        res.push(DIGITS[*b as usize & 0xF] as char);
        res.push(' ');
    }
    res.pop();
    res
}

/// A proxy encoder that allows adding and removing fields in the encoded
/// message, as helper for decoding tests.
pub trait TestingProxyEncoder<T> {
    /// Encodes `t` into a vector, adding an extra field.
    fn encode_with_extra_field(t: T) -> Result<Vec<u8>, serde_cbor::Error>;

    /// Encodes `t` into a vector, removing the field with the given index.
    fn encode_without_field(t: T, field: usize) -> Result<Vec<u8>, serde_cbor::Error>;
}

impl<T, M> TestingProxyEncoder<T> for M
where
    T: Into<M> + std::fmt::Debug,
    M: serde::Serialize,
{
    fn encode_with_extra_field(t: T) -> Result<Vec<u8>, serde_cbor::Error> {
        let add_field = |mut map: MapValue| {
            let prev = map.insert(Value::Integer(999), Value::Integer(999));
            assert!(
                prev.is_none(),
                "Expected no field with index 999, found {:?}",
                prev
            );
            map
        };

        encode_with_mutation(&t.into(), &add_field)
    }

    fn encode_without_field(t: T, field: usize) -> Result<Vec<u8>, serde_cbor::Error> {
        let remove_field = |mut map: MapValue| {
            map.remove(&Value::Integer(field as i128))
                .unwrap_or_else(|| panic!("No such field: {}", field));
            map
        };

        encode_with_mutation(&t.into(), &remove_field)
    }
}

type MapValue = BTreeMap<Value, Value>;

fn encode_with_mutation<T: serde::Serialize>(
    t: &T,
    mutate: &dyn Fn(MapValue) -> MapValue,
) -> Result<Vec<u8>, serde_cbor::Error> {
    let bytes = serde_cbor::ser::to_vec_packed(t)?;
    let value: Value = serde_cbor::de::from_slice(&bytes)?;

    let value = match value {
        Value::Map(map) => Value::Map(mutate(map)),
        other => panic!("Expected struct to serialize to a map, was {:?}", other),
    };

    serde_cbor::ser::to_vec_packed(&value)
}

//
// Own fixtures, to ensure that compatibility tests are self-contained.
//

pub fn request_message() -> RequestOrResponse {
    request().into()
}

fn request() -> Request {
    RequestBuilder::new()
        .receiver(canister_test_id(1))
        .sender(canister_test_id(2))
        .sender_reply_callback(CallbackId::from(3))
        .payment(cycles())
        .method_name("test".to_string())
        .method_payload(vec![6])
        .build()
}

fn response() -> Response {
    ResponseBuilder::new()
        .originator(canister_test_id(6))
        .respondent(canister_test_id(5))
        .originator_reply_callback(CallbackId::from(4))
        .refund(cycles())
        .response_payload(data_payload())
        .build()
}

fn funds() -> Funds {
    Funds::new(Cycles::new(3))
}

fn cycles() -> Cycles {
    Cycles::new(3)
}

fn data_payload() -> Payload {
    Payload::Data(vec![1])
}

fn reject_payload() -> Payload {
    Payload::Reject(reject_context())
}

fn reject_context() -> RejectContext {
    RejectContext {
        code: RejectCode::SysFatal,
        message: "Oops".into(),
    }
}
