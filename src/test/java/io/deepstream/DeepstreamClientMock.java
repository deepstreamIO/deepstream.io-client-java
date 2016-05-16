package io.deepstream;

import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;
import io.deepstream.message.Connection;

import java.util.ArrayList;

public class DeepstreamClientMock implements IDeepstreamClient {

    public ArrayList<ConnectionChangeListener> connectionListeners;

    public DeepstreamClientMock() {
        connectionListeners = new ArrayList<ConnectionChangeListener>();
    }

    public DeepstreamClientMock addConnectionChangeListener( ConnectionChangeListener connectionChangeListener ) {
        connectionListeners.add( connectionChangeListener );
        return this;
    }

    public DeepstreamClientMock removeConnectionChangeListener(ConnectionChangeListener connectionChangeListener) {
        return null;
    }

    public void onError(Topic topic, Event event, String message) {

    }
}
