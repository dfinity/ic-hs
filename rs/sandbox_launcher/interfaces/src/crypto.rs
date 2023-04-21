//! The crypto public interface.
mod keygen;

use ic_types::canister_http::CanisterHttpResponseMetadata;
pub use keygen::*;

mod errors;

pub use sign::threshold_sig::ni_dkg::{LoadTranscriptResult, NiDkgAlgorithm};

mod sign;

pub use sign::BasicSigVerifier;
pub use sign::BasicSigVerifierByPublicKey;
pub use sign::BasicSigner;
pub use sign::CanisterSigVerifier;
pub use sign::IngressSigVerifier;
pub use sign::MultiSigVerifier;
pub use sign::MultiSigner;
pub use sign::ThresholdSigVerifier;
pub use sign::ThresholdSigVerifierByPublicKey;
pub use sign::ThresholdSigner;

pub use sign::canister_threshold_sig::*;

use ic_types::consensus::certification::CertificationContent;
use ic_types::consensus::dkg as consensus_dkg;
use ic_types::consensus::{
    ecdsa::{EcdsaComplaintContent, EcdsaOpeningContent},
    Block, CatchUpContent, CatchUpContentProtobufBytes, FinalizationContent, NotarizationContent,
    RandomBeaconContent, RandomTapeContent,
};
use ic_types::crypto::canister_threshold_sig::idkg::{IDkgDealing, SignedIDkgDealing};
use ic_types::messages::{MessageId, WebAuthnEnvelope};

/// The functionality offered by the crypto component
pub trait Crypto:
    KeyManager
    // Block
    + BasicSigner<Block>
    + BasicSigVerifier<Block>
    // MessageId
    + BasicSigner<MessageId>
    // Dealing
    + BasicSigner<consensus_dkg::DealingContent>
    + BasicSigVerifier<consensus_dkg::DealingContent>
    // DKG
    + NiDkgAlgorithm
    // CertificationContent
    + ThresholdSigner<CertificationContent>
    + ThresholdSigVerifier<CertificationContent>
    + ThresholdSigVerifierByPublicKey<CertificationContent>
    // FinalizationContent
    + MultiSigner<FinalizationContent>
    + MultiSigVerifier<FinalizationContent>
    // NotarizationContent
    + MultiSigner<NotarizationContent>
    + MultiSigVerifier<NotarizationContent>
    // SignedIDkgDealing
    + BasicSigner<SignedIDkgDealing>
    + BasicSigVerifier<SignedIDkgDealing>
    // IDkgDealing
    + BasicSigner<IDkgDealing>
    + BasicSigVerifier<IDkgDealing>
    // EcdsaComplaintContent
    + BasicSigner<EcdsaComplaintContent>
    + BasicSigVerifier<EcdsaComplaintContent>
    // EcdsaOpeningContent
    + BasicSigner<EcdsaOpeningContent>
    + BasicSigVerifier<EcdsaOpeningContent>
    + IDkgProtocol
    + ThresholdEcdsaSigner
    + ThresholdEcdsaSigVerifier
    // CanisterHttpResponse
    + BasicSigner<CanisterHttpResponseMetadata>
    + BasicSigVerifier<CanisterHttpResponseMetadata>
    // RequestId/WebAuthn
    + BasicSigVerifierByPublicKey<MessageId>
    + BasicSigVerifierByPublicKey<WebAuthnEnvelope>
    // CatchUpPackage
    + ThresholdSigner<CatchUpContent>
    + ThresholdSigVerifier<CatchUpContent>
    + ThresholdSigVerifierByPublicKey<CatchUpContent>
    + ThresholdSigVerifierByPublicKey<CatchUpContentProtobufBytes>
    // RandomBeacon
    + ThresholdSigner<RandomBeaconContent>
    + ThresholdSigVerifier<RandomBeaconContent>
    // RandomTape
    + ThresholdSigner<RandomTapeContent>
    + ThresholdSigVerifier<RandomTapeContent>
    // Traits for signing/verifying a MerkleRoot
    // (both Multi- and ThresholdSig) will be added at a later stage.
    //
    // Also, further traits concerning other functionality of the crypto
    // component (such as key generation) will be added at a later stage.
{
}

/// A classifier for errors returned by the crypto component. Indicates whether
/// a given error is permanent and guaranteed to occur in all replicas.
pub trait ErrorReproducibility {
    // If true is returned, retrying the failing call will return the same error,
    // and the same error will be encountered by other replicas.
    fn is_reproducible(&self) -> bool;
}

// Blanket implementation of Crypto for all types that fulfill requirements
impl<T> Crypto for T where
    T: KeyManager
        + BasicSigner<Block>
        + BasicSigVerifier<Block>
        + BasicSigner<MessageId>
        + BasicSigner<consensus_dkg::DealingContent>
        + BasicSigVerifier<consensus_dkg::DealingContent>
        + NiDkgAlgorithm
        + ThresholdSigner<CertificationContent>
        + ThresholdSigVerifier<CertificationContent>
        + ThresholdSigVerifierByPublicKey<CertificationContent>
        + MultiSigner<FinalizationContent>
        + MultiSigVerifier<FinalizationContent>
        + MultiSigner<NotarizationContent>
        + MultiSigVerifier<NotarizationContent>
        + BasicSigner<SignedIDkgDealing>
        + BasicSigVerifier<SignedIDkgDealing>
        + BasicSigner<IDkgDealing>
        + BasicSigVerifier<IDkgDealing>
        + BasicSigner<EcdsaComplaintContent>
        + BasicSigVerifier<EcdsaComplaintContent>
        + BasicSigner<EcdsaOpeningContent>
        + BasicSigVerifier<EcdsaOpeningContent>
        + BasicSigner<CanisterHttpResponseMetadata>
        + BasicSigVerifier<CanisterHttpResponseMetadata>
        + IDkgProtocol
        + ThresholdEcdsaSigner
        + ThresholdEcdsaSigVerifier
        + BasicSigVerifierByPublicKey<MessageId>
        + BasicSigVerifierByPublicKey<WebAuthnEnvelope>
        + ThresholdSigner<CatchUpContent>
        + ThresholdSigVerifier<CatchUpContent>
        + ThresholdSigVerifierByPublicKey<CatchUpContent>
        + ThresholdSigVerifierByPublicKey<CatchUpContentProtobufBytes>
        + ThresholdSigner<RandomBeaconContent>
        + ThresholdSigVerifier<RandomBeaconContent>
        + ThresholdSigner<RandomTapeContent>
        + ThresholdSigVerifier<RandomTapeContent>
{
}
