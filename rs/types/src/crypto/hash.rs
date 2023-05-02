//! Defines hash types.

use crate::artifact::StateSyncMessage;
use crate::canister_http::{
    CanisterHttpResponse, CanisterHttpResponseMetadata, CanisterHttpResponseShare,
};
use crate::consensus::certification::CertificationMessage;
use crate::consensus::dkg as consensus_dkg;
use crate::consensus::{
    certification::{Certification, CertificationContent, CertificationShare},
    ecdsa::{
        EcdsaComplaintContent, EcdsaMessage, EcdsaOpeningContent, EcdsaSigShare, EcdsaTranscript,
    },
    Block, BlockPayload, CatchUpContent, CatchUpContentProtobufBytes, CatchUpShareContent,
    ConsensusMessage, FinalizationContent, HashedBlock, NotarizationContent, RandomBeaconContent,
    RandomTapeContent,
};
use crate::crypto::canister_threshold_sig::idkg::{
    IDkgDealing, IDkgDealingSupport, SignedIDkgDealing,
};
use crate::crypto::{CryptoHash, CryptoHashOf, Signed};
use crate::messages::{HttpCanisterUpdate, MessageId, SignedRequestBytes};
use crate::signature::{
    BasicSignature, MultiSignature, MultiSignatureShare, ThresholdSignature,
    ThresholdSignatureShare,
};
use ic_crypto_sha::{DomainSeparationContext, Sha256};
use std::hash::Hash;

#[cfg(test)]
mod tests;

/// The domain separator to be used when calculating the sender signature for a
/// request to the Internet Computer according to the
/// [interface specification](https://sdk.dfinity.org/docs/interface-spec/index.html).
pub const DOMAIN_IC_REQUEST: &[u8; 11] = b"\x0Aic-request";

pub(crate) const DOMAIN_NOTARIZATION_CONTENT: &str = "notarization_content_domain";
const DOMAIN_NOTARIZATION: &str = "notarization_domain";
const DOMAIN_NOTARIZATION_SHARE: &str = "notarization_share_domain";

pub(crate) const DOMAIN_FINALIZATION_CONTENT: &str = "finalization_content_domain";
const DOMAIN_FINALIZATION: &str = "finalization_domain";
const DOMAIN_FINALIZATION_SHARE: &str = "finalization_share_domain";

pub(crate) const DOMAIN_BLOCK: &str = "block_domain";
const DOMAIN_BLOCK_PROPOSAL: &str = "block_proposal_domain";

const DOMAIN_INMEMORY_PAYLOAD: &str = "inmemory_payload_domain";

pub(crate) const DOMAIN_RANDOM_BEACON_CONTENT: &str = "random_beacon_content_domain";
const DOMAIN_RANDOM_BEACON: &str = "random_beacon_domain";
const DOMAIN_RANDOM_BEACON_SHARE: &str = "random_beacon_share_domain";

pub(crate) const DOMAIN_CERTIFICATION_CONTENT: &str = "ic-state-root";
const DOMAIN_CERTIFICATION: &str = "certification_domain";
const DOMAIN_CERTIFICATION_SHARE: &str = "certification_share_domain";

pub(crate) const DOMAIN_DEALING_CONTENT: &str = "dealing_content_non_interactive";

const DOMAIN_DKG_MESSAGE: &str = "dkg_message_non_interactive";

const DOMAIN_HTTP_CANISTER_UPDATE: &str = "http_canister_update_domain";

const DOMAIN_SIGNED_REQUEST_BYTES: &str = "signed_request_bytes_domain";

const DOMAIN_MESSAGEID: &str = "messageid_domain";

pub(crate) const DOMAIN_IC_ONCHAIN_OBSERVABILITY_REPORT: &str =
    "ic-onchain-observability-report-domain";

pub(crate) const DOMAIN_RANDOM_TAPE_CONTENT: &str = "random_tape_content_domain";
const DOMAIN_RANDOM_TAPE: &str = "random_tape_domain";
const DOMAIN_RANDOM_TAPE_SHARE: &str = "random_tape_share_domain";

pub(crate) const DOMAIN_CATCH_UP_CONTENT: &str = "catch_up_content_domain";
const DOMAIN_CATCH_UP_CONTENT_PROTO: &str = "catch_up_content_proto_domain";
const DOMAIN_CATCH_UP_SHARE_CONTENT: &str = "catch_up_share_content_domain";
const DOMAIN_CATCH_UP_PACKAGE: &str = "catch_up_package_domain";
const DOMAIN_CATCH_UP_PACKAGE_SHARE: &str = "catch_up_package_share_domain";

const DOMAIN_STATE_SYNC_MESSAGE: &str = "state_sync_message_domain";
const DOMAIN_CONSENSUS_MESSAGE: &str = "consensus_message_domain";
const DOMAIN_CERTIFICATION_MESSAGE: &str = "certification_message_domain";
const DOMAIN_ECDSA_MESSAGE: &str = "ic-threshold-ecdsa-message-domain";
pub(crate) const DOMAIN_IDKG_DEALING: &str = "ic-idkg-dealing-domain";
pub(crate) const DOMAIN_SIGNED_IDKG_DEALING: &str = "ic-idkg-signed-dealing-domain";
const DOMAIN_IDKG_DEALING_SUPPORT: &str = "ic-idkg-dealing-support-domain";
const DOMAIN_ECDSA_TRANSCRIPT: &str = "ic-idkg-transcript-domain";
const DOMAIN_ECDSA_SIG_SHARE: &str = "ic-threshold-ecdsa-sig-share-domain";
pub(crate) const DOMAIN_ECDSA_COMPLAINT_CONTENT: &str =
    "ic-threshold-ecdsa-complaint-content-domain";
pub const DOMAIN_ECDSA_COMPLAINT: &str = "ic-threshold-ecdsa-complaint-domain";
pub(crate) const DOMAIN_ECDSA_OPENING_CONTENT: &str = "ic-threshold-ecdsa-opening-content-domain";
pub const DOMAIN_ECDSA_OPENING: &str = "ic-threshold-ecdsa-opening-domain";

pub(crate) const DOMAIN_CANISTER_HTTP_RESPONSE: &str = "ic-canister-http-response-domain";
pub(crate) const DOMAIN_CRYPTO_HASH_OF_CANISTER_HTTP_RESPONSE_METADATA: &str =
    "ic-crypto-hash-of-canister-http-response-metadata-domain";
pub(crate) const DOMAIN_CANISTER_HTTP_RESPONSE_SHARE: &str =
    "ic-canister-http-response-share-domain";

/// A type that specifies a domain for a cryptographic hash.
///
/// This trait is sealed and can only be implemented by types that are
/// explicitly approved by the Github owners of this file (that is, the
/// crypto team) via an implementation of the `CryptoHashDomainSeal`. Explicit
/// approval is required for security reasons to ensure proper domain
/// separation.
pub trait CryptoHashDomain: private::CryptoHashDomainSeal {
    /// Returns the domain separator used in cryptographic hashes.
    fn domain(&self) -> String;
}
mod private {

    use crate::canister_http::CanisterHttpResponseShare;
    use crate::crypto::canister_threshold_sig::idkg::{IDkgDealing, SignedIDkgDealing};

    use super::*;

    pub trait CryptoHashDomainSeal {}

    impl CryptoHashDomainSeal for NotarizationContent {}
    impl CryptoHashDomainSeal for Signed<NotarizationContent, MultiSignature<NotarizationContent>> {}
    impl CryptoHashDomainSeal
        for Signed<NotarizationContent, MultiSignatureShare<NotarizationContent>>
    {
    }

    impl CryptoHashDomainSeal for FinalizationContent {}
    impl CryptoHashDomainSeal for Signed<FinalizationContent, MultiSignature<FinalizationContent>> {}
    impl CryptoHashDomainSeal
        for Signed<FinalizationContent, MultiSignatureShare<FinalizationContent>>
    {
    }

    impl CryptoHashDomainSeal for Block {}
    impl CryptoHashDomainSeal for Signed<HashedBlock, BasicSignature<Block>> {}

    impl CryptoHashDomainSeal for BlockPayload {}

    impl CryptoHashDomainSeal for RandomBeaconContent {}
    impl CryptoHashDomainSeal for Signed<RandomBeaconContent, ThresholdSignature<RandomBeaconContent>> {}
    impl CryptoHashDomainSeal
        for Signed<RandomBeaconContent, ThresholdSignatureShare<RandomBeaconContent>>
    {
    }

    impl CryptoHashDomainSeal for CertificationContent {}
    impl CryptoHashDomainSeal for Certification {}
    impl CryptoHashDomainSeal for CertificationShare {}

    impl CryptoHashDomainSeal for consensus_dkg::Message {}
    impl CryptoHashDomainSeal for consensus_dkg::DealingContent {}

    impl CryptoHashDomainSeal for MessageId {}

    impl CryptoHashDomainSeal for HttpCanisterUpdate {}

    impl CryptoHashDomainSeal for SignedRequestBytes {}

    impl CryptoHashDomainSeal for RandomTapeContent {}
    impl CryptoHashDomainSeal for Signed<RandomTapeContent, ThresholdSignature<RandomTapeContent>> {}
    impl CryptoHashDomainSeal
        for Signed<RandomTapeContent, ThresholdSignatureShare<RandomTapeContent>>
    {
    }

    impl CryptoHashDomainSeal for CatchUpContent {}
    impl CryptoHashDomainSeal for CatchUpContentProtobufBytes {}
    impl CryptoHashDomainSeal for Signed<CatchUpContent, ThresholdSignature<CatchUpContent>> {}

    impl CryptoHashDomainSeal for CatchUpShareContent {}
    impl CryptoHashDomainSeal for Signed<CatchUpShareContent, ThresholdSignatureShare<CatchUpContent>> {}

    impl CryptoHashDomainSeal for StateSyncMessage {}
    impl CryptoHashDomainSeal for ConsensusMessage {}
    impl CryptoHashDomainSeal for CertificationMessage {}

    impl CryptoHashDomainSeal for EcdsaMessage {}

    impl CryptoHashDomainSeal for IDkgDealing {}

    impl CryptoHashDomainSeal for SignedIDkgDealing {}
    impl CryptoHashDomainSeal for IDkgDealingSupport {}

    impl CryptoHashDomainSeal for EcdsaTranscript {}
    impl CryptoHashDomainSeal for EcdsaSigShare {}

    impl CryptoHashDomainSeal for EcdsaComplaintContent {}
    impl CryptoHashDomainSeal for Signed<EcdsaComplaintContent, BasicSignature<EcdsaComplaintContent>> {}

    impl CryptoHashDomainSeal for EcdsaOpeningContent {}
    impl CryptoHashDomainSeal for Signed<EcdsaOpeningContent, BasicSignature<EcdsaOpeningContent>> {}

    impl CryptoHashDomainSeal for CanisterHttpResponse {}
    impl CryptoHashDomainSeal for CanisterHttpResponseMetadata {}
    impl CryptoHashDomainSeal for CanisterHttpResponseShare {}

    impl CryptoHashDomainSeal for CryptoHashableTestDummy {}
}

impl CryptoHashDomain for CanisterHttpResponse {
    fn domain(&self) -> String {
        DOMAIN_CANISTER_HTTP_RESPONSE.to_string()
    }
}

impl CryptoHashDomain for CanisterHttpResponseMetadata {
    fn domain(&self) -> String {
        DOMAIN_CRYPTO_HASH_OF_CANISTER_HTTP_RESPONSE_METADATA.to_string()
    }
}

impl CryptoHashDomain for CanisterHttpResponseShare {
    fn domain(&self) -> String {
        DOMAIN_CANISTER_HTTP_RESPONSE_SHARE.to_string()
    }
}

impl CryptoHashDomain for NotarizationContent {
    fn domain(&self) -> String {
        DOMAIN_NOTARIZATION_CONTENT.to_string()
    }
}

impl CryptoHashDomain for Signed<NotarizationContent, MultiSignature<NotarizationContent>> {
    fn domain(&self) -> String {
        DOMAIN_NOTARIZATION.to_string()
    }
}

impl CryptoHashDomain for Signed<NotarizationContent, MultiSignatureShare<NotarizationContent>> {
    fn domain(&self) -> String {
        DOMAIN_NOTARIZATION_SHARE.to_string()
    }
}

impl CryptoHashDomain for FinalizationContent {
    fn domain(&self) -> String {
        DOMAIN_FINALIZATION_CONTENT.to_string()
    }
}

impl CryptoHashDomain for Signed<FinalizationContent, MultiSignature<FinalizationContent>> {
    fn domain(&self) -> String {
        DOMAIN_FINALIZATION.to_string()
    }
}

impl CryptoHashDomain for Signed<FinalizationContent, MultiSignatureShare<FinalizationContent>> {
    fn domain(&self) -> String {
        DOMAIN_FINALIZATION_SHARE.to_string()
    }
}

impl CryptoHashDomain for CertificationContent {
    fn domain(&self) -> String {
        DOMAIN_CERTIFICATION_CONTENT.to_string()
    }
}

impl CryptoHashDomain for Certification {
    fn domain(&self) -> String {
        DOMAIN_CERTIFICATION.to_string()
    }
}

impl CryptoHashDomain for CertificationShare {
    fn domain(&self) -> String {
        DOMAIN_CERTIFICATION_SHARE.to_string()
    }
}

impl CryptoHashDomain for Block {
    fn domain(&self) -> String {
        DOMAIN_BLOCK.to_string()
    }
}

impl CryptoHashDomain for Signed<HashedBlock, BasicSignature<Block>> {
    fn domain(&self) -> String {
        DOMAIN_BLOCK_PROPOSAL.to_string()
    }
}

impl CryptoHashDomain for BlockPayload {
    fn domain(&self) -> String {
        DOMAIN_INMEMORY_PAYLOAD.to_string()
    }
}

impl CryptoHashDomain for RandomBeaconContent {
    fn domain(&self) -> String {
        DOMAIN_RANDOM_BEACON_CONTENT.to_string()
    }
}

impl CryptoHashDomain for Signed<RandomBeaconContent, ThresholdSignature<RandomBeaconContent>> {
    fn domain(&self) -> String {
        DOMAIN_RANDOM_BEACON.to_string()
    }
}

impl CryptoHashDomain
    for Signed<RandomBeaconContent, ThresholdSignatureShare<RandomBeaconContent>>
{
    fn domain(&self) -> String {
        DOMAIN_RANDOM_BEACON_SHARE.to_string()
    }
}

impl CryptoHashDomain for consensus_dkg::DealingContent {
    fn domain(&self) -> String {
        DOMAIN_DEALING_CONTENT.to_string()
    }
}

impl CryptoHashDomain for consensus_dkg::Message {
    fn domain(&self) -> String {
        DOMAIN_DKG_MESSAGE.to_string()
    }
}

impl CryptoHashDomain for HttpCanisterUpdate {
    fn domain(&self) -> String {
        DOMAIN_HTTP_CANISTER_UPDATE.to_string()
    }
}

impl CryptoHashDomain for SignedRequestBytes {
    fn domain(&self) -> String {
        DOMAIN_SIGNED_REQUEST_BYTES.to_string()
    }
}

impl CryptoHashDomain for MessageId {
    fn domain(&self) -> String {
        DOMAIN_MESSAGEID.to_string()
    }
}

impl CryptoHashDomain for RandomTapeContent {
    fn domain(&self) -> String {
        DOMAIN_RANDOM_TAPE_CONTENT.to_string()
    }
}

impl CryptoHashDomain for Signed<RandomTapeContent, ThresholdSignature<RandomTapeContent>> {
    fn domain(&self) -> String {
        DOMAIN_RANDOM_TAPE.to_string()
    }
}

impl CryptoHashDomain for Signed<RandomTapeContent, ThresholdSignatureShare<RandomTapeContent>> {
    fn domain(&self) -> String {
        DOMAIN_RANDOM_TAPE_SHARE.to_string()
    }
}

impl CryptoHashDomain for CatchUpContent {
    fn domain(&self) -> String {
        DOMAIN_CATCH_UP_CONTENT.to_string()
    }
}

impl CryptoHashDomain for CatchUpContentProtobufBytes {
    fn domain(&self) -> String {
        DOMAIN_CATCH_UP_CONTENT_PROTO.to_string()
    }
}

impl CryptoHashDomain for CatchUpShareContent {
    fn domain(&self) -> String {
        DOMAIN_CATCH_UP_SHARE_CONTENT.to_string()
    }
}

impl CryptoHashDomain for Signed<CatchUpContent, ThresholdSignature<CatchUpContent>> {
    fn domain(&self) -> String {
        DOMAIN_CATCH_UP_PACKAGE.to_string()
    }
}

impl CryptoHashDomain for Signed<CatchUpShareContent, ThresholdSignatureShare<CatchUpContent>> {
    fn domain(&self) -> String {
        DOMAIN_CATCH_UP_PACKAGE_SHARE.to_string()
    }
}

impl CryptoHashDomain for StateSyncMessage {
    fn domain(&self) -> String {
        DOMAIN_STATE_SYNC_MESSAGE.to_string()
    }
}

impl CryptoHashDomain for ConsensusMessage {
    fn domain(&self) -> String {
        DOMAIN_CONSENSUS_MESSAGE.to_string()
    }
}

impl CryptoHashDomain for CertificationMessage {
    fn domain(&self) -> String {
        DOMAIN_CERTIFICATION_MESSAGE.to_string()
    }
}

impl CryptoHashDomain for EcdsaMessage {
    fn domain(&self) -> String {
        DOMAIN_ECDSA_MESSAGE.to_string()
    }
}

impl CryptoHashDomain for IDkgDealing {
    fn domain(&self) -> String {
        DOMAIN_IDKG_DEALING.to_string()
    }
}

impl CryptoHashDomain for SignedIDkgDealing {
    fn domain(&self) -> String {
        DOMAIN_SIGNED_IDKG_DEALING.to_string()
    }
}

impl CryptoHashDomain for IDkgDealingSupport {
    fn domain(&self) -> String {
        DOMAIN_IDKG_DEALING_SUPPORT.to_string()
    }
}

impl CryptoHashDomain for EcdsaTranscript {
    fn domain(&self) -> String {
        DOMAIN_ECDSA_TRANSCRIPT.to_string()
    }
}

impl CryptoHashDomain for EcdsaSigShare {
    fn domain(&self) -> String {
        DOMAIN_ECDSA_SIG_SHARE.to_string()
    }
}

impl CryptoHashDomain for EcdsaComplaintContent {
    fn domain(&self) -> String {
        DOMAIN_ECDSA_COMPLAINT_CONTENT.to_string()
    }
}

impl CryptoHashDomain for Signed<EcdsaComplaintContent, BasicSignature<EcdsaComplaintContent>> {
    fn domain(&self) -> String {
        DOMAIN_ECDSA_COMPLAINT.to_string()
    }
}

impl CryptoHashDomain for EcdsaOpeningContent {
    fn domain(&self) -> String {
        DOMAIN_ECDSA_OPENING_CONTENT.to_string()
    }
}

impl CryptoHashDomain for Signed<EcdsaOpeningContent, BasicSignature<EcdsaOpeningContent>> {
    fn domain(&self) -> String {
        DOMAIN_ECDSA_OPENING.to_string()
    }
}

impl CryptoHashDomain for CryptoHashableTestDummy {
    fn domain(&self) -> String {
        "test_struct_domain".to_string()
    }
}

/// A helper struct for testing that implements `CryptoHashable`.
///
/// It is defined here because the struct must implement the `CryptoHashDomain`
/// trait, which is _sealed_ and must only be implemented here in this crate.
/// Ideally, this struct would be annotated with `#[cfg(test)]` so that it is
/// only available in test code, however, then it would not be visible outside
/// of this crate where it is needed.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CryptoHashableTestDummy(pub Vec<u8>);

/// A cryptographically hashable type.
pub trait CryptoHashable: CryptoHashDomain + Hash {}
impl<T> CryptoHashable for T where T: CryptoHashDomain + Hash {}

/// Creates a (typed) domain-separated cryptographic hash.
///
/// The bytes that are hashed are a combination of
/// * the byte representation of the hash domain obtained via `CryptoHashable`s
///   supertrait `CryptoHashDomain`
/// * the bytes fed to the hasher state via `CryptoHashable`s supertrait `Hash`
///
/// Note that the trait `CryptoHashDomain` is sealed for security reasons. To
/// implement this trait for a new struct that shall be cryptographically
/// hashed, contact the crypto team.
///
/// The (secure) hashing algorithm that is used internally is intentionally
/// unspecified because it may be subject to change across registry/protocol
/// versions. Use `Sha256` instead if the algorithm used for producing
/// the hash must not change across registry/protocol versions.
pub fn crypto_hash<T: CryptoHashable>(data: &T) -> CryptoHashOf<T> {
    let mut hash = Sha256::new_with_context(&DomainSeparationContext::new(data.domain()));
    data.hash(&mut hash);
    CryptoHashOf::new(CryptoHash(hash.finish().to_vec()))
}
