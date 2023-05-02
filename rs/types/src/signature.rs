use crate::{crypto::threshold_sig::ni_dkg::NiDkgId, crypto::*, CountBytes, NodeId};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// BasicSignature captures basic signature on a value and the identity of the
/// replica that signed it
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, Ord, PartialOrd)]
pub struct BasicSignature<T> {
    pub signature: BasicSigOf<T>,
    pub signer: NodeId,
}

impl<T> CountBytes for BasicSignature<T> {
    fn count_bytes(&self) -> usize {
        self.signature.get_ref().count_bytes() + std::mem::size_of::<NodeId>()
    }
}

/// `BasicSigned<T>` captures a value of type T and a BasicSignature on it
pub type BasicSigned<T> = Signed<T, BasicSignature<T>>;

/// BasicSignatureBatch captures a collection of basic signatures on the same value and
/// the identities of the replicas that signed it.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, Ord, PartialOrd)]
pub struct BasicSignatureBatch<T> {
    pub signatures_map: BTreeMap<NodeId, BasicSigOf<T>>,
}

/// ThresholdSignature captures a threshold signature on a value and the
/// DKG id of the threshold key material used to sign
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ThresholdSignature<T> {
    pub signature: CombinedThresholdSigOf<T>,
    pub signer: NiDkgId,
}

/// ThresholdSignatureShare captures a share of a threshold signature on a value
/// and the identity of the replica that signed
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, Ord, PartialOrd)]
pub struct ThresholdSignatureShare<T> {
    pub signature: ThresholdSigShareOf<T>,
    pub signer: NodeId,
}

/// MultiSignature captures a cryptographic multi-signature, which is one
/// message signed by multiple signers
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, Ord, PartialOrd)]
pub struct MultiSignature<T> {
    pub signature: CombinedMultiSigOf<T>,
    pub signers: Vec<NodeId>,
}

/// MultiSignatureShare is a signature from one replica. Multiple shares can be
/// aggregated into a MultiSignature.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub struct MultiSignatureShare<T> {
    pub signature: IndividualMultiSigOf<T>,
    pub signer: NodeId,
}
