//! The artifact pool public interface.
use crate::time_source::TimeSource;
use derive_more::From;
use ic_types::{
    artifact::{ArtifactKind, PriorityFn},
    replica_version::ReplicaVersion,
    CountBytes, NodeId, Time,
};
use serde::{Deserialize, Serialize};

pub trait ChangeSetProducer<Pool>: Send {
    type ChangeSet;

    /// Inspect the input [Pool] to build a [ChangeSet] of actions to
    /// be executed.
    ///
    /// The caller is then expected to apply the returned [ChangeSet] to the
    /// input of this call, namely a mutable version of the [Pool]. The reason
    /// that P2P clients (e.g. consensus) do not directly mutate the objects are:
    ///
    /// 1. The actual mutation may need to be coupled with other things,
    /// performed in a single transaction, and so on. So it is better to leave
    /// it to the caller to decide.
    ///
    /// 2. Because [Pool] is passed as an read-only reference, the
    /// caller is free to run other readers concurrently should it choose to.
    /// But this is a minor point.
    fn on_state_change(&self, pool: &Pool) -> Self::ChangeSet;
}

/// The trait defines the canonical way for mutating an artifact pool.
/// There should be one owner of the object implementing this trait.
pub trait MutablePool<Artifact: ArtifactKind, C> {
    /// Inserts a message into the unvalidated part of the pool.
    fn insert(&mut self, msg: UnvalidatedArtifact<Artifact::Message>);

    /// Removes a message from the unvalidated part of the pool.
    fn remove(&mut self, _id: &Artifact::Id) {
        unimplemented!()
    }

    /// Applies a set of change actions to the pool.
    fn apply_changes(&mut self, time_source: &dyn TimeSource, change_set: C);
}

/// Consensus to gossip interface.
pub trait PriorityFnAndFilterProducer<Artifact: ArtifactKind, Pool>: Send + Sync {
    /// Return a priority function that matches the given consensus pool.
    fn get_priority_function(&self, pool: &Pool) -> PriorityFn<Artifact::Id, Artifact::Attribute>;

    /// Return a filter that represents what artifacts are needed.
    fn get_filter(&self) -> Artifact::Filter {
        Artifact::Filter::default()
    }
}

/// ValidatedPoolReader trait is the generic interface used by P2P to interact
/// with the validated portion of an artifact pool without resulting in any mutations.
/// Every pool needs to implement this trait.
pub trait ValidatedPoolReader<T: ArtifactKind> {
    /// Check if an artifact exists by its Id.
    fn contains(&self, id: &T::Id) -> bool;

    /// Get a validated artifact by its identifier
    ///
    /// #Returns:
    /// - 'Some`: Artifact from the validated pool.
    /// - `None`: Artifact does not exist in the validated pool.
    fn get_validated_by_identifier(&self, id: &T::Id) -> Option<T::Message>;

    /// Get all validated artifacts by the filter
    /// See interfaces/src/artifact_manager.rs for more details
    ///
    /// #Returns:
    /// A iterator over all the validated artifacts.
    fn get_all_validated_by_filter(
        &self,
        filter: &T::Filter,
    ) -> Box<dyn Iterator<Item = T::Message> + '_>;
}

/// Contains different errors that can happen on artifact acceptance check.
/// In our P2P protocol none of the errors from 'ArtifactPoolError' are
/// handled by the caller. So the enum is used only for tracking different
/// rejection reasons.
#[derive(Debug, From)]
pub enum ArtifactPoolError {
    /// Message has expired.
    MessageExpired,
    /// Message expiry is too far in the future.
    MessageExpiryTooLong,
    /// Error when artifact version is not accepted.
    ArtifactReplicaVersionError(ReplicaVersionMismatch),
}

/// Describe expected version and artifact version when there is a mismatch.
#[derive(Debug)]
pub struct ReplicaVersionMismatch {
    pub expected: ReplicaVersion,
    pub artifact: ReplicaVersion,
}

/// Validated artifact
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ValidatedArtifact<T> {
    pub msg: T,
    pub timestamp: Time,
}

impl<T> ValidatedArtifact<T> {
    pub fn map<U, F>(self, f: F) -> ValidatedArtifact<U>
    where
        F: FnOnce(T) -> U,
    {
        ValidatedArtifact {
            msg: f(self.msg),
            timestamp: self.timestamp,
        }
    }
}

/// Unvalidated artifact
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct UnvalidatedArtifact<T> {
    pub message: T,
    pub peer_id: NodeId,
    pub timestamp: Time,
}

// Traits for accessing data for (un)validated artifacts follow.

impl<T: CountBytes> CountBytes for ValidatedArtifact<T> {
    fn count_bytes(&self) -> usize {
        self.msg.count_bytes() + self.timestamp.count_bytes()
    }
}

impl<T> AsRef<T> for ValidatedArtifact<T> {
    fn as_ref(&self) -> &T {
        &self.msg
    }
}

impl<T> AsRef<T> for UnvalidatedArtifact<T> {
    fn as_ref(&self) -> &T {
        &self.message
    }
}

/// A trait similar to Into, but without its restrictions.
pub trait IntoInner<T>: AsRef<T> {
    fn into_inner(self) -> T;
}

impl<T> IntoInner<T> for ValidatedArtifact<T> {
    fn into_inner(self) -> T {
        self.msg
    }
}

impl<T> IntoInner<T> for UnvalidatedArtifact<T> {
    fn into_inner(self) -> T {
        self.message
    }
}

/// A trait to get timestamp.
pub trait HasTimestamp {
    fn timestamp(&self) -> Time;
}

impl<T> HasTimestamp for ValidatedArtifact<T> {
    fn timestamp(&self) -> Time {
        self.timestamp
    }
}

impl<T> HasTimestamp for UnvalidatedArtifact<T> {
    fn timestamp(&self) -> Time {
        self.timestamp
    }
}
