package io.deepstream;

import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

abstract class IDeepstreamClient {
    private UtilAckTimeoutRegistry utilAckTimeoutRegistry;

    abstract IDeepstreamClient addConnectionChangeListener(ConnectionChangeListener connectionChangeListener);
    abstract IDeepstreamClient removeConnectionChangeListener( ConnectionChangeListener connectionChangeListener );
    abstract void onError(Topic topic, Event event, String message);
    abstract ConnectionState getConnectionState();
    abstract String getUid();

    UtilAckTimeoutRegistry getAckTimeoutRegistry( IDeepstreamClient client ) {
        if(  utilAckTimeoutRegistry == null ) {
            utilAckTimeoutRegistry = new UtilAckTimeoutRegistry( client );
        }
        return utilAckTimeoutRegistry;
    }
}
