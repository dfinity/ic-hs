//! Traits providing the crypto component interfaces for a threshold ECDSA protocol.
//!
//! A set of nodes can use a threshold ECDSA protocol to establish a decentralized ECDSA signing service.
//! The protocol is secure as long as the number of corrupted nodes is less than the threshold (typically
//! less than 1/3). The main building block used in the protocol is an interactive distributed key generation
//! protocol (IDKG), which is used for the following:
//! * Generate an ECDSA signing key, which is secret-shared between the participants.
//! * Re-share an existing ECDSA signing key to a new set of nodes, e.g. if nodes leave or join a subnet, or
//!   to back up the key on a another subnet.
//! * Secret-share random values (also known as Beaver triplets) used in the computation of threshold ECDSA
//!   signatures.
//!
//! A signing service supporting multiple users needs to manage many public keys, often several per users.
//! Since generating and managing these is expensive, it would not scale to generate different keys per user.
//! Instead, the threshold ECDSA protocol implemented by the IC uses key derivation to derive individual
//! users public keys from a master ECDSA signing key. The key derivation is a generalization of BIP32, and
//! users can further derive any number of subkeys from their main public key. This allows the signing service
//! to easily scale with the number of users.
//!
//! At an high-level, the ECDSA signing protocol can be divided into an offline and online phase:
//! * Offline phase: this is a pre-computation step that can be performed ahead of time, before the message and
//!   the identity of the signer are known to the nodes. Most of the interaction takes place in this phase of the
//!   protocol. This phase consists of running 5 instances of the IDKG protocol to construct a *quadruple* of
//!   secret-shared values that are used in the online phase of the protocol (the value generated in one of the
//!   instances is used as an intermediary value and it is not used in the signing protocol). Each quadruple can
//!   only be used in the construction of a single ECDSA signature.
//! * Online phase: this phase is executed to answer an incoming signature request, i.e. once the message and
//!   the identity of the signer are known. This part of the protocol is non-interactive, i.e all nodes use their
//!   shares of a precomputed quadruple to compute locally a signature share. Enough shares can then be publicly
//!   combined into a full ECDSA signature.

use ic_base_types::NodeId;
use ic_types::crypto::canister_threshold_sig::error::{
    IDkgCreateDealingError, IDkgCreateTranscriptError, IDkgLoadTranscriptError,
    IDkgOpenTranscriptError, IDkgRetainKeysError, IDkgVerifyComplaintError,
    IDkgVerifyDealingPrivateError, IDkgVerifyDealingPublicError, IDkgVerifyInitialDealingsError,
    IDkgVerifyOpeningError, IDkgVerifyTranscriptError, ThresholdEcdsaCombineSigSharesError,
    ThresholdEcdsaSignShareError, ThresholdEcdsaVerifyCombinedSignatureError,
    ThresholdEcdsaVerifySigShareError,
};
use ic_types::crypto::canister_threshold_sig::idkg::{
    BatchSignedIDkgDealing, IDkgComplaint, IDkgOpening, IDkgTranscript, IDkgTranscriptParams,
    InitialIDkgDealings, SignedIDkgDealing,
};
use ic_types::crypto::canister_threshold_sig::{
    ThresholdEcdsaCombinedSignature, ThresholdEcdsaSigInputs, ThresholdEcdsaSigShare,
};
use std::collections::{BTreeMap, HashSet};

/// A Crypto Component interface to run interactive distributed key generation (IDKG) protocol as part of the
/// threshold ECDSA protocol. Nodes can engage in an instance of the IDKG protocol to secret-share some values.
/// A successful execution of the protocol terminates with a *transcript* which summarizes the messages exchanged
/// during the protocol.
///
/// # Protocol Overview:
/// The IDKG protocol involves two sets of participants interacting in the protocol: *dealers* and *receivers*.
/// The set of dealers jointly constructs a common secret, which is secret-shared to the set of the receivers.
/// I.e. each receiver obtains a piece of the common secret, such that a threshold number of receivers has to
/// contribute with their shares in order to reconstruct the full secret.
///
/// Before starting the protocol, the node in a subnet agree on some global parameters [`IDkgTranscriptParams`],
/// including, for example, the set of participants, which elliptic curve should be used, and an identifier for the
/// protocol instance. The params used in a protocol instance must remain the same for the entire duration of the
/// protocol.
///
/// ## Dealings:
/// Dealers contribute to the protocol with a *dealing*, which consists of encrypted shares of a secret value known
/// by the dealer, one encrypted share for each receiver, as well as a commitment to the shares. The protocol uses
/// different types of commitments schemes:
/// * Perfectly hiding commitments, as used in Pedersen's  verifiable secret sharing scheme (VSS). Here we refer to
///   values committed with such commitment as `Masked`.
/// * Perfectly binding commitments, as used in Feldman's VSS. Here we refer to values committed with such commitment
///   as `Unmasked`.
///
/// Dealers can construct dealings for different kind of secrets and use different types of commitment schemes:
/// * [`Random`]: at the end of the protocol the receivers have a share of a new random value that is masked.
/// * [`ReshareOfMasked`]: given an existing masked secret shared between the dealers, at the end the receivers obtain
///   a new sharing for the same secret, but unmasked.
/// * [`ReshareOfUnmasked`]: given an existing unmasked secret shared between the dealers, at the end the receivers
///   obtain a new sharing for the same secret, that is still unmasked.
/// * [`UnmaskedTimesMasked`]: given two existing secrets, one masked and one unmasked, shared between the dealers,
///   at the end the receivers obtain a sharing of the product of the initial secret that is masked.
///
/// Since this is an *interactive* distributed key generation protocol, the verification of encrypted shares in a dealing
/// is done privately by the receivers, which decrypt their shares and check the validity against some public commitment.
/// This is in contrast with non-interactive DKG protocols, where the verification of dealings can be performed publicly.
/// In case the verification is successful, receivers sign the dealings to show their support.
///
/// ## Transcripts:
/// Multiple dealings contributions from different dealers are required to establish a common secret. The minimum
/// number of contributions depends both on the reconstruction threshold and on the kind of secret value being shared.
/// Once enough dealings with sufficient receivers' support are collected, they are then combined into a transcript
/// [`IDkgTranscript`] which summarizes all the information the receivers need to reconstruct their share of the
/// common secret.
///
/// ## Complaints:
/// After a transcript is successfully created, it could happen that a receiver cannot successfully decrypt all its
/// shares from the dealers. If this happens the receiver can compute a [`IDkgComplaint`] against specific dealings
/// and send this to all the other receivers. The receivers can verify the complaint and, if valid, return an
/// [`IDkgOpening`] to the issuer of the complaint. Given enough valid openings, the issuer can reconstruct the missing
/// share.
///
/// # Use-Cases
///
/// ## Key Generation
///
/// * Dealers: all the nodes in a subnet.
/// * Receivers: same nodes as the set of dealers.
/// The nodes run two instances of the IDKG protocol in sequence:
/// 1. Run IDKG protocol to generate a [`Random`] secret key. Since the commitment used here is masking, the nodes do
///    not yet know the corresponding public key, but only a share of it.
/// 2. Run IDKG protocol to do a [`ReshareOfMasked`] secret key generated in the previous protocol instance. Since the
///    commitment used here is unmasked, the nodes will then learn the public key corresponding to the secret key
///    generated in the first instance.
///
/// ## Key Re-sharing
///
/// When the membership of subnet changes or in case shares associated with the key needs to be refreshed (e.g. for proactive
/// security), the nodes of the subnet run one instance of the IDKG protocol.
/// * Dealers: nodes that were receivers in the IDKG instance that generated the key to be re-shared.
/// * Receivers: all nodes in the subnet with the new topology (which may include new nodes and exclude nodes that were
///   removed).
/// The nodes re-share a key by running a single IDKG protocol instance:
/// 1. Run IDKG protocol to do a [`ReshareOfUnmasked`] key that was previously generated.
///
/// ## XNet Key Re-sharing
///
/// Subnets can share an existing key with another subnet, e.g. to back it up or to scale the threshold ECDSA protocol.
/// Half of the IDKG protocol runs in the source subnet that knows the key, while the other half runs in a target subnet
/// receiving a copy of the key.
/// * Dealers: all nodes in the source subnet
/// * Receivers: all nodes in the target subnet. Note that this subnet does not need to exist yet, it could be under
///   construction
///
/// The nodes run a single IDKG protocol instance:
/// * The source subnet initiate a protocol to [`ReshareOfUnmasked`] key that was previously generated.
/// * The source subnet collects enough dealings into [`InitialIDkgDealings`] that are then included in the registry.
/// * The target subnet fetches the [`InitialIDkgDealings`] from the registry and completes the execution of the protocol.
///
/// No communication between the source and target subnets is required apart from the registry. This is possible by
/// making sure that the initial dealings include enough honest dealings, so that the receivers can terminate the protocol.
/// I.e. with at most f corruptions, a set of >=2f+1 dealings from distinct dealers is guaranteed to have at least f+1
/// honest dealings.
///
/// [`InitialIDkgDealings`]: ic_types::crypto::canister_threshold_sig::idkg::InitialIDkgDealings
/// [`PreSignatureQuadruple`]: ic_types::crypto::canister_threshold_sig::PreSignatureQuadruple
/// [`Random`]: ic_types::crypto::canister_threshold_sig::idkg::IDkgTranscriptOperation::Random
/// [`ReshareOfMasked`]: ic_types::crypto::canister_threshold_sig::idkg::IDkgTranscriptOperation::ReshareOfMasked
/// [`ReshareOfUnmasked`]: ic_types::crypto::canister_threshold_sig::idkg::IDkgTranscriptOperation::ReshareOfUnmasked
/// [`UnmaskedTimesMasked`]: ic_types::crypto::canister_threshold_sig::idkg::IDkgTranscriptOperation::UnmaskedTimesMasked
///
/// # Preconditions
///
/// * For a fixed `IDkgTranscriptId`, the `IDkgTranscriptParams` must never
///   change throughout a round of execution. That is, if two calls to methods
///   of `IDkgProtocol` are made with `IDkgTranscriptParams` values `params1`
///   and `params2` respectively, then if `params1.transcript_id == params2.dkg_id`,
///   we must have `params1 == params2`.
pub trait IDkgProtocol {
    /// Create a signed dealing of a prescribed type..
    ///
    /// A dealing contains a polynomial commitment and encryption of the secret
    /// shares of the receivers.
    /// In addition, for some transcript types, this contains a contextual proof
    /// for the secret value being shared.
    /// The dealing is signed by the secret key of the node issuing the dealing (a.k.a the dealer).
    ///
    /// The type of dealing created is determined by the
    /// `IDkgTranscriptOperation` specified in the `params`.
    ///
    /// For resharing or multiplication, the relevant previous dealings
    /// must have been loaded via prior calls to `load_transcript`.
    fn create_dealing(
        &self,
        params: &IDkgTranscriptParams,
    ) -> Result<SignedIDkgDealing, IDkgCreateDealingError>;

    /// Perform public verification of a dealing.
    ///
    /// This checks the consistency of the dealing with the params, the signature on the dealing,
    /// and it verifies the optional contextual proof.
    ///
    /// # Errors
    /// * `IDkgVerifyDealingPublicError::TranscriptIdMismatch` if the transcript ID in the `params`
    ///    is different from the one included in the dealing.
    /// * `IDkgVerifyDealingPublicError::InvalidDealing` if the internal dealing is invalid.
    /// * `IDkgVerifyDealingPublicError::InvalidSignature` if the signature on the dealing is invalid.
    fn verify_dealing_public(
        &self,
        params: &IDkgTranscriptParams,
        signed_dealing: &SignedIDkgDealing,
    ) -> Result<(), IDkgVerifyDealingPublicError>;

    /// Perform private verification of a dealing.
    ///
    /// If called by a receiver of the dealing, this verifies:
    /// * Decryptability of the receiver's ciphertext
    /// * The consistency of the decrypted share with the polynomial
    ///   commitment.
    ///
    /// # Preconditions
    /// * Public verification of the given dealing was successful: call first
    ///   [Self::verify_dealing_public] and ensure that no error occurred *before* calling this method.
    ///   Otherwise, calling this method may result in a security vulnerability!
    ///
    /// # Errors
    /// * `IDkgVerifyDealingPrivateError::NotAReceiver` if the caller isn't in the
    ///   dealing's receivers. Only receivers can perform private verification of dealings.
    /// * `IDkgVerifyDealingPrivateError::InvalidDealing` if the decrypted shares are not consistent
    ///    with polynomial commitment.
    /// * `IDkgVerifyDealingPrivateError::InvalidArgument` if some argument cannot be parsed correctly.
    /// * `IDkgVerifyDealingPrivateError::PrivateKeyNotFound` if the secret key store of the node
    ///    does not contain the secret key necessary to decrypt the ciphertext.
    /// * `IDkgVerifyDealingPrivateError::RegistryError` if the registry client returned an error.
    /// * `IDkgVerifyDealingPrivateError::PublicKeyNotInRegistry` if the encryption key of the
    ///    receiver is not in the registry.
    /// * `IDkgVerifyDealingPrivateError::MalformedPublicKey` if the public key of one of the receivers
    ///    is not well formed.
    /// * `IDkgVerifyDealingPrivateError::UnsupportedAlgorithm` if the `params.algorithm_id` is not supported
    /// * `IDkgVerifyDealingPrivateError::InternalError` if the an internal error occurs.
    /// * `IDkgVerifyDealingPrivateError::CspVaultRpcError` if there is an RPC error reported when
    ///    connecting with the vault.
    fn verify_dealing_private(
        &self,
        params: &IDkgTranscriptParams,
        signed_dealing: &SignedIDkgDealing,
    ) -> Result<(), IDkgVerifyDealingPrivateError>;

    /// Verifies initial dealings for XNet resharing.
    ///
    /// Verification ensures that
    /// * the `params` are equal to the params of `initial_dealings`
    /// *  public dealing verification is successful for all dealings in `initial_dealings`
    ///
    /// # Errors
    /// * `IDkgVerifyInitialDealingsError::MismatchingTranscriptParams` if the
    ///   `params` are equal to the params of `initial_dealings`.
    /// * `IDkgVerifyInitialDealingsError::PublicVerificationFailure` if public
    ///   dealing verification fails for some dealing in `initial_dealings`.
    fn verify_initial_dealings(
        &self,
        params: &IDkgTranscriptParams,
        initial_dealings: &InitialIDkgDealings,
    ) -> Result<(), IDkgVerifyInitialDealingsError>;

    /// Combine the given dealings into a transcript.
    ///
    /// Performs the following on each dealing:
    /// * Checks consistency with the params
    /// * Checks that the multisignature was computed by at least
    /// `IDkgTranscriptParams::verification_threshold` receivers
    /// * Verifies the (combined) multisignature
    fn create_transcript(
        &self,
        params: &IDkgTranscriptParams,
        dealings: &BTreeMap<NodeId, BatchSignedIDkgDealing>,
    ) -> Result<IDkgTranscript, IDkgCreateTranscriptError>;

    /// Verify the multisignature on each dealing in the transcript.
    ///
    /// Also checks that each multisignature was computed by at least
    /// `IDkgTranscriptParams::verification_threshold` receivers.
    fn verify_transcript(
        &self,
        params: &IDkgTranscriptParams,
        transcript: &IDkgTranscript,
    ) -> Result<(), IDkgVerifyTranscriptError>;

    /// Load the transcript.
    ///
    /// This:
    /// * Decrypts this receiver's ciphertext in each dealing
    /// * Checks the consistency of the decrypted shares with the polynomial
    ///   commitment
    /// * Recombines the secret share from all dealers' contributions
    /// * Combines the polynomial commitments to get any needed public data
    /// * Stores the recombined secret in the local canister secret key store
    ///
    /// # Returns
    /// * `Ok([])` if decryption succeeded
    /// * `Ok(Vec<IDkgComplaints>)` if some dealings require Openings
    /// * `Err` if a fatal error occurred
    fn load_transcript(
        &self,
        transcript: &IDkgTranscript,
    ) -> Result<Vec<IDkgComplaint>, IDkgLoadTranscriptError>;

    /// Verifies the validity of a complaint against some dealings.
    ///
    /// This:
    /// * Checks the decryption verification proof
    /// * Attempts decryption-from-proof of the complainer's ciphertext and
    ///   either:
    ///   * Confirms that the ciphertext can't be decrypted
    ///   * Checks that the decrypted share is not consistent with the
    ///     polynomial commitment.
    ///
    /// # Errors
    /// * `IDkgVerifyComplaintError::InvalidComplaint` if the complaint is invalid.
    /// * `IDkgVerifyComplaintError::InvalidArguments` if one or more arguments
    ///   are invalid.
    /// * `IDkgVerifyComplaintError::InvalidArgumentsMismatchingTranscriptIDs` if
    ///   the transcript IDs in the transcript and the complaint do not match (i.e.,
    ///   are not equal).
    /// * `IDkgVerifyComplaintError::InvalidArgumentsMissingDealingInTranscript`
    ///   if the (verified) dealings in the transcript do not contain a dealing
    ///   whose dealer ID matches the complaint's dealer ID.
    /// * `IDkgVerifyComplaintError::InvalidArgumentsMissingComplainerInTranscript`
    ///   if the transcript's receivers do not contain a receiver whose ID matches
    ///   the complaint's complainer ID.
    /// * `IDkgVerifyComplaintError::ComplainerPublicKeyNotInRegistry` if the
    ///   complainer's (MEGa) public key cannot be found in the registry.
    /// * `IDkgVerifyComplaintError::MalformedComplainerPublicKey` if the
    ///   complainer's (MEGa) public key fetched from the registry is malformed.
    /// * `IDkgVerifyComplaintError::UnsupportedComplainerPublicKeyAlgorithm` if
    ///   the algorithm of the complainer's (MEGa) public key in the registry is
    ///   not supported.
    /// * `IDkgVerifyComplaintError::SerializationError` if the (internal raw)
    ///   complaint cannot be deserialized, or if the dealing corresponding to the
    ///   complaint's dealer ID cannot be deserialized from the transcript.
    /// * `IDkgVerifyComplaintError::Registry` if the registry client returns an
    ///   error, e.g., because the transcript's `registry_version` is not available.
    /// * `IDkgVerifyComplaintError::InternalError` if an internal error occurred
    ///   during the verification.
    fn verify_complaint(
        &self,
        transcript: &IDkgTranscript,
        complainer_id: NodeId,
        complaint: &IDkgComplaint,
    ) -> Result<(), IDkgVerifyComplaintError>;

    /// Generate an opening for the dealing given in `complaint`,
    /// reported by `complainer_id`.
    fn open_transcript(
        &self,
        transcript: &IDkgTranscript,
        complainer_id: NodeId,
        complaint: &IDkgComplaint,
    ) -> Result<IDkgOpening, IDkgOpenTranscriptError>;

    /// Verify that an opening corresponds to the complaint,
    /// and matches the commitment in the transcript.
    fn verify_opening(
        &self,
        transcript: &IDkgTranscript,
        opener: NodeId,
        opening: &IDkgOpening,
        complaint: &IDkgComplaint,
    ) -> Result<(), IDkgVerifyOpeningError>;

    /// Load the transcript (cf. `load_transcript`),
    /// with the help of `openings`.
    ///
    /// # Preconditions
    /// * For each (complaint, (opener, opening)) tuple, it holds that
    ///   `verify_opening(transcript, opener, opening, complaint).is_ok()`
    fn load_transcript_with_openings(
        &self,
        transcript: &IDkgTranscript,
        openings: &BTreeMap<IDkgComplaint, BTreeMap<NodeId, IDkgOpening>>,
    ) -> Result<(), IDkgLoadTranscriptError>;

    /// Retains only the IDKG key material needed for the given transcripts.
    /// If no transcript is given, no key material will be removed.
    ///
    /// All other IDKG key material will be removed as follows:
    /// * rotated IDKG public keys in the public key store which are no longer used.
    ///   The oldest used IDKG public key is identified by the smallest registry version
    ///   in the given transcripts. Older IDKG public key will be removed while the others
    ///   will be kept.
    /// * corresponding IDKG secret keys in the node secret key store
    /// * IDKG threshold keys in the canister secret key store which are no longer used.
    ///   Each given transcript uniquely identifies an IDKG threshold key.
    ///   IDKG threshold keys not identified by a transcript will be removed.
    ///
    /// # Errors
    /// * `IDkgRetainThresholdKeysError::InternalError` if an internal error such as
    ///   an RPC error communicating with a remote CSP vault occurs
    /// * `IDkgRetainThresholdKeysError::SerializationError` if a transcript cannot
    ///   be serialized into a key id to identify the IDKG threshold secret key
    fn retain_active_transcripts(
        &self,
        active_transcripts: &HashSet<IDkgTranscript>,
    ) -> Result<(), IDkgRetainKeysError>;
}

/// A Crypto Component interface to generate ECDSA threshold signature shares.
pub trait ThresholdEcdsaSigner {
    /// Generate a signature share.
    fn sign_share(
        &self,
        inputs: &ThresholdEcdsaSigInputs,
    ) -> Result<ThresholdEcdsaSigShare, ThresholdEcdsaSignShareError>;
}

/// A Crypto Component interface to perform public operations in the ECDSA
/// threshold signature scheme.
pub trait ThresholdEcdsaSigVerifier {
    /// Verify that the given signature share was correctly created from
    /// `inputs`.
    fn verify_sig_share(
        &self,
        signer: NodeId,
        inputs: &ThresholdEcdsaSigInputs,
        share: &ThresholdEcdsaSigShare,
    ) -> Result<(), ThresholdEcdsaVerifySigShareError>;

    /// Combine the given signature shares into a conventional ECDSA signature.
    ///
    /// The signature is returned as raw bytes.
    fn combine_sig_shares(
        &self,
        inputs: &ThresholdEcdsaSigInputs,
        shares: &BTreeMap<NodeId, ThresholdEcdsaSigShare>,
    ) -> Result<ThresholdEcdsaCombinedSignature, ThresholdEcdsaCombineSigSharesError>;

    /// Verify that a combined signature was properly created from the inputs.
    fn verify_combined_sig(
        &self,
        inputs: &ThresholdEcdsaSigInputs,
        signature: &ThresholdEcdsaCombinedSignature,
    ) -> Result<(), ThresholdEcdsaVerifyCombinedSignatureError>;
}
