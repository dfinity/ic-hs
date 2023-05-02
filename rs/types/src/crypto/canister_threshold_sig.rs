//! Defines canister threshold signature types.
use crate::crypto::canister_threshold_sig::idkg::{
    IDkgMaskedTranscriptOrigin, IDkgReceivers, IDkgTranscript, IDkgTranscriptType,
    IDkgUnmaskedTranscriptOrigin,
};
use crate::crypto::AlgorithmId;
use crate::{NumberOfNodes, Randomness};
use core::fmt;
use ic_base_types::PrincipalId;
use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter};

pub mod error;
pub mod idkg;

#[cfg(test)]
mod tests;

/// A threshold ECDSA public key.
///
/// The public key itself is stored as raw bytes.
///
/// The chain key is included for BIP32-style key derivation
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct EcdsaPublicKey {
    pub algorithm_id: AlgorithmId,
    #[serde(with = "serde_bytes")]
    pub public_key: Vec<u8>,
    #[serde(with = "serde_bytes")]
    pub chain_key: Vec<u8>,
}

/// A threshold ECDSA public key.
///
/// The public key itself is stored as raw bytes.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct MasterEcdsaPublicKey {
    pub algorithm_id: AlgorithmId,
    #[serde(with = "serde_bytes")]
    pub public_key: Vec<u8>,
}

/// A combined threshold ECDSA signature.
///
/// The signature itself is stored as raw bytes.
#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ThresholdEcdsaCombinedSignature {
    #[serde(with = "serde_bytes")]
    pub signature: Vec<u8>,
}

impl Display for ThresholdEcdsaCombinedSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl fmt::Debug for ThresholdEcdsaCombinedSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "ThresholdEcdsaCombinedSignature {{ signature: 0x{} }}",
            hex::encode(&self.signature)
        )
    }
}

/// Quadruple of IDKG transcripts consumed by a canister-requested threshold signature.
/// Each quadruple MUST be used *at most once* for a signature. Otherwise, the private key may be
/// leaked!
///
/// Each signature, in addition to the transcript for the sharing of the private key, requires the
/// following 4 transcripts that may be pre-computed (they are independent of the message to be
/// signed):
/// * an unmasked transcript for sharing of a random value `kappa`
/// * a masked transcript for sharing of another random value `lambda`
/// * a masked transcript for sharing the value `kappa * lambda`
/// * a masked transcript for sharing the value `private_key * lambda`
#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PreSignatureQuadruple {
    kappa_unmasked: IDkgTranscript,
    lambda_masked: IDkgTranscript,
    kappa_times_lambda: IDkgTranscript,
    key_times_lambda: IDkgTranscript,
}

impl Display for PreSignatureQuadruple {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl fmt::Debug for PreSignatureQuadruple {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "PreSignatureQuadruple {{ ")?;
        write!(f, "kappa_unmasked: {:?}", self.kappa_unmasked.transcript_id)?;
        write!(f, ", lambda_masked: {:?}", self.lambda_masked.transcript_id)?;
        write!(
            f,
            ", kappa_times_lambda: {:?}",
            self.kappa_times_lambda.transcript_id
        )?;
        write!(
            f,
            ", key_times_lambda: {:?}",
            self.key_times_lambda.transcript_id
        )?;
        write!(f, " }}")?;
        Ok(())
    }
}

impl PreSignatureQuadruple {
    /// Creates a `PreSignatureQuadruple` which is a collection of four transcripts
    /// that can be used in the threshold ECDSA protocol.
    ///
    /// A `PreSignatureQuadruple` can only be created if the following invariants hold:
    /// * All transcripts use the same algorithm ID (error: `InconsistentAlgorithms`)
    /// * All transcripts have the same receiver set (error: `InconsistentReceivers`)
    /// * The `kappa_unmasked` transcript is of type `Unmasked` with origin
    ///   `ReshareMasked` (error: `InvalidTranscriptOrigin`)
    /// * The `lambda_masked` transcript is of type `Masked` with origin
    ///   `Random` (error: `InvalidTranscriptOrigin`)
    /// * The `kappa_times_lambda` transcript is of type `Masked` with origin
    ///   `UnmaskedTimesMasked(left,right)`, where `left` and `right` are the
    ///   transcript IDs of `kappa_unmasked` and `lambda_masked`, respectively (error: `InvalidTranscriptOrigin`)
    /// * The `key_times_lambda` transcript is of type `Masked` with origin
    ///   `UnmaskedTimesMasked(left,right)`, where `right` is the
    ///   transcript ID of `lambda_masked` (error: `InvalidTranscriptOrigin`)
    pub fn new(
        kappa_unmasked: IDkgTranscript,
        lambda_masked: IDkgTranscript,
        kappa_times_lambda: IDkgTranscript,
        key_times_lambda: IDkgTranscript,
    ) -> Result<Self, error::PresignatureQuadrupleCreationError> {
        Self::check_algorithm_ids(
            &kappa_unmasked,
            &lambda_masked,
            &kappa_times_lambda,
            &key_times_lambda,
        )?;
        Self::check_receivers_are_equal(
            &kappa_unmasked,
            &lambda_masked,
            &kappa_times_lambda,
            &key_times_lambda,
        )?;
        Self::check_consistency_of_transcripts(
            &kappa_unmasked,
            &lambda_masked,
            &kappa_times_lambda,
            &key_times_lambda,
        )?;

        Ok(Self {
            kappa_unmasked,
            lambda_masked,
            kappa_times_lambda,
            key_times_lambda,
        })
    }

    pub fn kappa_unmasked(&self) -> &IDkgTranscript {
        &self.kappa_unmasked
    }

    pub fn lambda_masked(&self) -> &IDkgTranscript {
        &self.lambda_masked
    }

    pub fn kappa_times_lambda(&self) -> &IDkgTranscript {
        &self.kappa_times_lambda
    }

    pub fn key_times_lambda(&self) -> &IDkgTranscript {
        &self.key_times_lambda
    }

    fn check_algorithm_ids(
        kappa_unmasked: &IDkgTranscript,
        lambda_masked: &IDkgTranscript,
        kappa_times_lambda: &IDkgTranscript,
        key_times_lambda: &IDkgTranscript,
    ) -> Result<(), error::PresignatureQuadrupleCreationError> {
        if kappa_unmasked.algorithm_id == lambda_masked.algorithm_id
            && lambda_masked.algorithm_id == kappa_times_lambda.algorithm_id
            && kappa_times_lambda.algorithm_id == key_times_lambda.algorithm_id
        {
            Ok(())
        } else {
            Err(error::PresignatureQuadrupleCreationError::InconsistentAlgorithmIds)
        }
    }

    fn check_receivers_are_equal(
        kappa_unmasked: &IDkgTranscript,
        lambda_masked: &IDkgTranscript,
        kappa_times_lambda: &IDkgTranscript,
        key_times_lambda: &IDkgTranscript,
    ) -> Result<(), error::PresignatureQuadrupleCreationError> {
        if kappa_unmasked.receivers == lambda_masked.receivers
            && lambda_masked.receivers == kappa_times_lambda.receivers
            && kappa_times_lambda.receivers == key_times_lambda.receivers
        {
            Ok(())
        } else {
            Err(error::PresignatureQuadrupleCreationError::InconsistentReceivers)
        }
    }

    fn check_consistency_of_transcripts(
        kappa_unmasked: &IDkgTranscript,
        lambda_masked: &IDkgTranscript,
        kappa_times_lambda: &IDkgTranscript,
        key_times_lambda: &IDkgTranscript,
    ) -> Result<(), error::PresignatureQuadrupleCreationError> {
        Self::check_kappa_unmasked_origin(kappa_unmasked)?;
        Self::check_lambda_masked_origin(lambda_masked)?;
        Self::check_kappa_times_lambda_origin(kappa_unmasked, lambda_masked, kappa_times_lambda)?;
        Self::check_key_times_lambda_origin(lambda_masked, key_times_lambda)?;
        Ok(())
    }

    fn check_kappa_unmasked_origin(
        kappa_unmasked: &IDkgTranscript,
    ) -> Result<(), error::PresignatureQuadrupleCreationError> {
        match &kappa_unmasked.transcript_type {
            IDkgTranscriptType::Unmasked(IDkgUnmaskedTranscriptOrigin::ReshareMasked(_)) => Ok(()),
            _ => Err(error::PresignatureQuadrupleCreationError::InvalidTranscriptOrigin(
                format!("`kappa_unmasked` transcript expected to have type `Unmasked` with `ReshareMasked` origin, but found transcript of type {:?}", kappa_unmasked.transcript_type))
            ),
        }
    }

    fn check_lambda_masked_origin(
        lambda_masked: &IDkgTranscript,
    ) -> Result<(), error::PresignatureQuadrupleCreationError> {
        match &lambda_masked.transcript_type {
            IDkgTranscriptType::Masked(IDkgMaskedTranscriptOrigin::Random) => Ok(()),
            _ => Err(error::PresignatureQuadrupleCreationError::InvalidTranscriptOrigin(
                format!("`lambda_masked` transcript expected to have type `Masked` with `Random` origin, but found transcript of type {:?}", lambda_masked.transcript_type))
            ),
        }
    }

    fn check_kappa_times_lambda_origin(
        kappa_unmasked: &IDkgTranscript,
        lambda_masked: &IDkgTranscript,
        kappa_times_lambda: &IDkgTranscript,
    ) -> Result<(), error::PresignatureQuadrupleCreationError> {
        match &kappa_times_lambda.transcript_type {
            IDkgTranscriptType::Masked(IDkgMaskedTranscriptOrigin::UnmaskedTimesMasked(id_l, id_r))
            if *id_l == kappa_unmasked.transcript_id && *id_r == lambda_masked.transcript_id
            => {
                Ok(())
            }
            _ => Err(error::PresignatureQuadrupleCreationError::InvalidTranscriptOrigin(
                format!("`kappa_times_lambda` transcript expected to have type `Masked` with origin of type `UnmaskedTimesMasked({:?},{:?})`, but found transcript of type {:?}", kappa_unmasked.transcript_id, lambda_masked.transcript_id, kappa_times_lambda.transcript_type))
            ),
        }
    }

    fn check_key_times_lambda_origin(
        lambda_masked: &IDkgTranscript,
        key_times_lambda: &IDkgTranscript,
    ) -> Result<(), error::PresignatureQuadrupleCreationError> {
        match &key_times_lambda.transcript_type {
            IDkgTranscriptType::Masked(IDkgMaskedTranscriptOrigin::UnmaskedTimesMasked(_, id_r, ))
            if *id_r == lambda_masked.transcript_id => {
                Ok(())
            }
            _ => Err(error::PresignatureQuadrupleCreationError::InvalidTranscriptOrigin(
                format!("`key_times_lambda` transcript expected to have type `Masked` with origin of type `UnmaskedTimesMasked(_,{:?})`, but found transcript of type {:?}", lambda_masked.transcript_id, key_times_lambda.transcript_type))
            ),
        }
    }
}

/// Metadata used to derive a specific ECDSA keypair.
#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ExtendedDerivationPath {
    pub caller: PrincipalId,
    pub derivation_path: Vec<Vec<u8>>,
}

impl Display for ExtendedDerivationPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl fmt::Debug for ExtendedDerivationPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "ExtendedDerivationPath {{ caller: {:?}", self.caller)?;
        write!(f, ", derivation_path: {{ ")?;
        let mut first_path = true;
        for path in &self.derivation_path {
            if !first_path {
                write!(f, ", ")?;
                first_path = false;
            }
            write!(f, "{}", hex::encode(path))?;
        }
        write!(f, " }}")?;
        write!(f, " }}")?;
        Ok(())
    }
}

/// All inputs required to generate a canister threshold signature.
#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ThresholdEcdsaSigInputs {
    derivation_path: ExtendedDerivationPath,
    #[serde(with = "serde_bytes")]
    hashed_message: Vec<u8>,
    nonce: Randomness,
    presig_quadruple: PreSignatureQuadruple,
    key_transcript: IDkgTranscript,
}

// The byte length of an hashed message for ECDSA signatures over the curve secp256k1.
pub const ECDSA_SECP256K1_HASH_BYTE_LENGTH: usize = 32;

impl Display for ThresholdEcdsaSigInputs {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl fmt::Debug for ThresholdEcdsaSigInputs {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "ThresholdEcdsaSigInputs {{ ")?;
        write!(f, "derivation_path: {:?}", self.derivation_path)?;
        write!(
            f,
            ", hashed_message: 0x{}",
            hex::encode(&self.hashed_message)
        )?;
        write!(f, ", nonce: 0x{}", hex::encode(self.nonce.as_ref()))?;
        write!(f, ", presig_quadruple: {}", self.presig_quadruple)?;
        write!(f, ", key_transcript: {}", self.key_transcript.transcript_id)?;
        write!(f, " }}")?;
        Ok(())
    }
}

impl ThresholdEcdsaSigInputs {
    /// Creates the inputs to the threshold ECDSA signing protocol.
    ///
    /// A `ThresholdEcdsaSigInputs` can only be created if the following invariants hold:
    /// * The algorithm ID of the `key_transcript` is the same as the algorithm ID
    ///   of the transcripts in the `presig_quadruple` (error: `InconsistentAlgorithms`)
    /// * The algorithm ID of the `key_transcript` is supported for the creation
    ///   of threshold ECDSA signatures (error: `UnsupportedAlgorithm`)
    /// * The length of the `hashed_message` is correct for the algorithm ID
    ///   of the `key_transcript` (error: `InvalidHashLength`).
    /// * All transcripts have the same receiver set (error: `InconsistentReceivers`)
    /// * The `key_times_lambda` transcript of the `presig_quadruple` is the product
    ///   of the `key_transcript` and another masked transcript (error: `InvalidQuadrupleOrigin`)
    pub fn new(
        derivation_path: &ExtendedDerivationPath,
        hashed_message: &[u8],
        nonce: Randomness,
        presig_quadruple: PreSignatureQuadruple,
        key_transcript: IDkgTranscript,
    ) -> Result<Self, error::ThresholdEcdsaSigInputsCreationError> {
        Self::check_algorithm_ids(&presig_quadruple, &key_transcript)?;
        Self::check_hash_length(hashed_message, key_transcript.algorithm_id)?;
        Self::check_receivers_are_equal(&presig_quadruple, &key_transcript)?;
        Self::check_quadruple_origin(&presig_quadruple, &key_transcript)?;

        Ok(Self {
            derivation_path: derivation_path.clone(),
            hashed_message: hashed_message.to_vec(),
            nonce,
            presig_quadruple,
            key_transcript,
        })
    }

    pub fn derivation_path(&self) -> &ExtendedDerivationPath {
        &self.derivation_path
    }

    pub fn hashed_message(&self) -> &[u8] {
        &self.hashed_message
    }

    pub fn nonce(&self) -> &Randomness {
        &self.nonce
    }

    pub fn presig_quadruple(&self) -> &PreSignatureQuadruple {
        &self.presig_quadruple
    }

    pub fn key_transcript(&self) -> &IDkgTranscript {
        &self.key_transcript
    }

    /// Number of contributions needed to reconstruct a sharing.
    pub fn reconstruction_threshold(&self) -> NumberOfNodes {
        // We already checked that all receiver sets are equal
        self.key_transcript.reconstruction_threshold()
    }

    pub fn receivers(&self) -> &IDkgReceivers {
        // We already checked that all receiver sets are equal
        &self.key_transcript.receivers
    }

    pub fn algorithm_id(&self) -> AlgorithmId {
        // We already checked that all transcripts have the same alg_id
        self.key_transcript.algorithm_id
    }

    fn check_algorithm_ids(
        presig_quadruple: &PreSignatureQuadruple,
        key_transcript: &IDkgTranscript,
    ) -> Result<(), error::ThresholdEcdsaSigInputsCreationError> {
        // The quadruple was already checked to have a consistent algorithm ID
        if presig_quadruple.kappa_unmasked().algorithm_id == key_transcript.algorithm_id {
            Ok(())
        } else {
            Err(error::ThresholdEcdsaSigInputsCreationError::InconsistentAlgorithmIds)
        }
    }

    fn check_hash_length(
        hashed_message: &[u8],
        algorithm_id: AlgorithmId,
    ) -> Result<(), error::ThresholdEcdsaSigInputsCreationError> {
        match algorithm_id {
            AlgorithmId::ThresholdEcdsaSecp256k1 => {
                if hashed_message.len() != ECDSA_SECP256K1_HASH_BYTE_LENGTH {
                    return Err(error::ThresholdEcdsaSigInputsCreationError::InvalidHashLength);
                }
                Ok(())
            }
            _ => Err(error::ThresholdEcdsaSigInputsCreationError::UnsupportedAlgorithm),
        }
    }

    fn check_receivers_are_equal(
        presig_quadruple: &PreSignatureQuadruple,
        key_transcript: &IDkgTranscript,
    ) -> Result<(), error::ThresholdEcdsaSigInputsCreationError> {
        // The quadruple was already checked to have a consistent receiver set
        if presig_quadruple.kappa_unmasked().receivers == key_transcript.receivers {
            Ok(())
        } else {
            Err(error::ThresholdEcdsaSigInputsCreationError::InconsistentReceivers)
        }
    }

    fn check_quadruple_origin(
        presig_quadruple: &PreSignatureQuadruple,
        key_transcript: &IDkgTranscript,
    ) -> Result<(), error::ThresholdEcdsaSigInputsCreationError> {
        match &presig_quadruple.key_times_lambda.transcript_type {
            IDkgTranscriptType::Masked(IDkgMaskedTranscriptOrigin::UnmaskedTimesMasked(
                key_id_from_mult,
                _,
            )) if *key_id_from_mult == key_transcript.transcript_id => Ok(()),
            _ => Err(error::ThresholdEcdsaSigInputsCreationError::InvalidQuadrupleOrigin(
                format!("Quadruple transcript `key_times_lambda` expected to have type `Masked` with origin of type `UnmaskedTimesMasked({:?},_)`, but found transcript of type {:?}", key_transcript.transcript_id, presig_quadruple.key_times_lambda.transcript_type))
            ),
        }
    }
}

/// A single threshold ECDSA signature share.
#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ThresholdEcdsaSigShare {
    #[serde(with = "serde_bytes")]
    pub sig_share_raw: Vec<u8>,
}

impl Display for ThresholdEcdsaSigShare {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl fmt::Debug for ThresholdEcdsaSigShare {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ThresholdEcdsaSigShare {{ sig_share_raw: 0x{} }}",
            hex::encode(&self.sig_share_raw)
        )
    }
}
