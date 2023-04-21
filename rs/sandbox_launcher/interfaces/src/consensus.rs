//! The consensus public interface.
use crate::{
    canister_http::{
        CanisterHttpPayloadValidationError, CanisterHttpPermanentValidationError,
        CanisterHttpTransientValidationError,
    },
    ingress_manager::{
        IngressPayloadValidationError, IngressPermanentError, IngressTransientError,
    },
    messaging::{InvalidXNetPayload, XNetPayloadValidationError, XNetTransientValidationError},
    self_validating_payload::{
        InvalidSelfValidatingPayload, SelfValidatingPayloadValidationError,
        SelfValidatingTransientValidationError,
    },
    validation::{ValidationError, ValidationResult},
};
use ic_base_types::{NumBytes, SubnetId};
use ic_types::{
    batch::{BatchPayload, ValidationContext},
    consensus::{block_maker::SubnetRecords, Payload},
    registry::RegistryClientError,
    Height, Time,
};

/// The [`PayloadBuilder`] is responsible for creating and validating payload that
/// is included in consensus blocks.
pub trait PayloadBuilder: Send + Sync {
    /// Produces a payload that is valid given `past_payloads` and `context`.
    ///
    /// `past_payloads` contains the `Payloads` from all blocks above the
    /// certified height provided in `context`, in descending block height
    /// order.
    fn get_payload(
        &self,
        height: Height,
        past_payloads: &[(Height, Time, Payload)],
        context: &ValidationContext,
        subnet_records: &SubnetRecords,
    ) -> BatchPayload;

    /// Checks whether the provided `payload` is valid given `past_payloads` and
    /// `context`.
    ///
    /// `past_payloads` contains the `Payloads` from all blocks above the
    /// certified height provided in `context`, in descending block height
    /// order.
    fn validate_payload(
        &self,
        height: Height,
        payload: &Payload,
        past_payloads: &[(Height, Time, Payload)],
        context: &ValidationContext,
    ) -> ValidationResult<PayloadValidationError>;
}

#[derive(Debug)]
pub enum PayloadPermanentError {
    XNetPayloadValidationError(InvalidXNetPayload),
    IngressPayloadValidationError(IngressPermanentError),
    PayloadTooBig {
        expected: NumBytes,
        received: NumBytes,
    },
    SelfValidatingPayloadValidationError(InvalidSelfValidatingPayload),
    CanisterHttpPayloadValidationError(CanisterHttpPermanentValidationError),
}

#[derive(Debug)]
pub enum PayloadTransientError {
    XNetPayloadValidationError(XNetTransientValidationError),
    IngressPayloadValidationError(IngressTransientError),
    RegistryUnavailable(RegistryClientError),
    SubnetNotFound(SubnetId),
    SelfValidatingPayloadValidationError(SelfValidatingTransientValidationError),
    CanisterHttpPayloadValidationError(CanisterHttpTransientValidationError),
}

/// Payload validation error
pub type PayloadValidationError = ValidationError<PayloadPermanentError, PayloadTransientError>;

impl From<IngressPayloadValidationError> for PayloadValidationError {
    fn from(err: IngressPayloadValidationError) -> Self {
        err.map(
            PayloadPermanentError::IngressPayloadValidationError,
            PayloadTransientError::IngressPayloadValidationError,
        )
    }
}

impl From<XNetPayloadValidationError> for PayloadValidationError {
    fn from(err: XNetPayloadValidationError) -> Self {
        err.map(
            PayloadPermanentError::XNetPayloadValidationError,
            PayloadTransientError::XNetPayloadValidationError,
        )
    }
}

impl From<SelfValidatingPayloadValidationError> for PayloadValidationError {
    fn from(err: SelfValidatingPayloadValidationError) -> Self {
        err.map(
            PayloadPermanentError::SelfValidatingPayloadValidationError,
            PayloadTransientError::SelfValidatingPayloadValidationError,
        )
    }
}

impl From<CanisterHttpPayloadValidationError> for PayloadValidationError {
    fn from(err: CanisterHttpPayloadValidationError) -> Self {
        err.map(
            PayloadPermanentError::CanisterHttpPayloadValidationError,
            PayloadTransientError::CanisterHttpPayloadValidationError,
        )
    }
}
