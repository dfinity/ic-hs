use crate::messages::Blob;
use crate::{
    messages::{
        http::{representation_indepent_hash_call_or_query, CallOrQuery},
        HasCanisterId, HttpRequestError, HttpUserQuery, MessageId,
    },
    CanisterId, PrincipalId, UserId,
};
use ic_error_types::RejectCode;
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;

/// Represents a Query that is sent by an end user to a canister.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct UserQuery {
    pub source: UserId,
    pub receiver: CanisterId,
    pub method_name: String,
    pub method_payload: Vec<u8>,
    pub ingress_expiry: u64,
    pub nonce: Option<Vec<u8>>,
}

impl UserQuery {
    pub fn id(&self) -> MessageId {
        MessageId::from(representation_indepent_hash_call_or_query(
            CallOrQuery::Query,
            self.receiver.get().into_vec(),
            &self.method_name,
            self.method_payload.clone(),
            self.ingress_expiry,
            self.source.get().into_vec(),
            self.nonce.as_deref(),
        ))
    }
}

impl TryFrom<HttpUserQuery> for UserQuery {
    type Error = HttpRequestError;

    fn try_from(query: HttpUserQuery) -> Result<Self, Self::Error> {
        Ok(Self {
            source: UserId::from(PrincipalId::try_from(query.sender.0).map_err(|err| {
                HttpRequestError::InvalidPrincipalId(format!(
                    "Converting sender to PrincipalId failed with {}",
                    err
                ))
            })?),
            receiver: CanisterId::try_from(query.canister_id.0).map_err(|err| {
                HttpRequestError::InvalidPrincipalId(format!(
                    "Converting canister_id to PrincipalId failed with {:?}",
                    err
                ))
            })?,
            method_name: query.method_name,
            method_payload: query.arg.0,
            ingress_expiry: query.ingress_expiry,
            nonce: query.nonce.map(|n| n.0),
        })
    }
}

impl HasCanisterId for UserQuery {
    fn canister_id(&self) -> CanisterId {
        self.receiver
    }
}

/// Represents a Query that is sent by the IC.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct AnonymousQuery {
    pub receiver: CanisterId,
    pub method_name: String,
    pub method_payload: Vec<u8>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
#[serde(tag = "status")]
pub enum AnonymousQueryResponse {
    Replied {
        reply: AnonymousQueryResponseReply,
    },
    Rejected {
        reject_code: RejectCode,
        reject_message: String,
    },
}

/// The body of the `AnonymousQueryResponse`.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct AnonymousQueryResponseReply {
    pub arg: Blob,
}

#[cfg(test)]
mod test {
    use super::super::{Blob, HttpUserQuery};
    use maplit::btreemap;
    use serde::Deserialize;
    use serde_cbor::Value;

    fn bytes(bytes: &[u8]) -> Value {
        Value::Bytes(bytes.to_vec())
    }

    fn text(text: &'static str) -> Value {
        Value::Text(text.to_string())
    }

    fn number(value: u64) -> Value {
        Value::Integer(value.into())
    }

    /// Makes sure that `val` deserializes to `obj`
    /// Used when testing _incoming_ messages from the HTTP Handler's point of
    /// view
    fn assert_cbor_de_equal<T>(obj: &T, val: Value)
    where
        for<'de> T: Deserialize<'de> + std::fmt::Debug + std::cmp::Eq,
    {
        let obj2 = serde_cbor::value::from_value(val).expect("Could not read CBOR value");
        assert_eq!(*obj, obj2);
    }

    #[test]
    fn decoding_read_query() {
        assert_cbor_de_equal(
            &HttpUserQuery {
                arg: Blob(vec![]),
                canister_id: Blob(vec![42; 8]),
                method_name: "some_method_name".to_string(),
                sender: Blob(vec![0x04]),
                nonce: None,
                ingress_expiry: 0,
            },
            Value::Map(btreemap! {
                text("arg") => bytes(&[][..]),
                text("canister_id") => bytes(&[42; 8][..]),
                text("method_name") => text("some_method_name"),
                text("sender") => bytes(&[0x04][..]),
                text("ingress_expiry") => number(0),
            }),
        );
    }

    #[test]
    fn decoding_read_query_arg() {
        assert_cbor_de_equal(
            &HttpUserQuery {
                arg: Blob(b"Hello, World!".to_vec()),
                canister_id: Blob(vec![42; 8]),
                method_name: "some_method_name".to_string(),
                sender: Blob(vec![0; 33]),
                nonce: None,
                ingress_expiry: 0,
            },
            Value::Map(btreemap! {
                text("arg") => bytes(b"Hello, World!"),
                text("canister_id") => bytes(&[42; 8][..]),
                text("method_name") => text("some_method_name"),
                text("sender") => bytes(&[0; 33]),
                text("ingress_expiry") => number(0),
            }),
        );
    }
}
