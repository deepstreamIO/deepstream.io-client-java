package io.deepstream;

import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

public interface IDeepstreamClient {
    IDeepstreamClient addConnectionChangeListener(ConnectionChangeListener connectionChangeListener);
    IDeepstreamClient removeConnectionChangeListener( ConnectionChangeListener connectionChangeListener );
    void onError(Topic topic, Event event, String message);
    ConnectionState getConnectionState();
}
