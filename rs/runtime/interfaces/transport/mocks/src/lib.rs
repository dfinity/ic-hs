use ic_base_types::{NodeId, RegistryVersion};
use ic_interfaces_transport::{
    Transport, TransportChannelId, TransportError, TransportEventHandler, TransportPayload,
};
use mockall::*;
use std::net::SocketAddr;

mock! {
    pub Transport {}

    impl Transport for Transport {
        fn set_event_handler(
            &self,
            event_handler: TransportEventHandler,
        );

        fn start_connection(
            &self,
            peer_id: &NodeId,
            peer_addr: SocketAddr,
            lastest_registry_version: RegistryVersion,
            earliest_registry_version: RegistryVersion,
        );

        fn stop_connection(
            &self,
            peer: &NodeId,
        );

        fn send(
            &self,
            peer: &NodeId,
            channel_id: TransportChannelId,
            message: TransportPayload,
        ) -> Result<(), TransportError>;

        fn clear_send_queues(
            &self,
            peer: &NodeId,
        );
    }
}

mock! {
    pub TranportEventHandler {}
}
