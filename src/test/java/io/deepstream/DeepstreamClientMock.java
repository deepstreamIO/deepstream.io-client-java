package io.deepstream;

import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.util.ArrayList;

public class DeepstreamClientMock implements IDeepstreamClient {

    public ArrayList<ConnectionChangeListener> connectionListeners;
    private ConnectionState connectionState = ConnectionState.CLOSED;

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

    public ConnectionState getConnectionState() {
        return this.connectionState;
    }

    public void setConnectionState( ConnectionState state ) {
        this.connectionState = state;
        for ( ConnectionChangeListener listener : this.connectionListeners ) {
            listener.connectionStateChanged( state );
        }
    }
}
