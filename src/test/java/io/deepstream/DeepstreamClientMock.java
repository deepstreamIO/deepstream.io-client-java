package io.deepstream;

import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.util.ArrayList;

public class DeepstreamClientMock extends IDeepstreamClient implements ErrorCallback {

    private ArrayList<ConnectionChangeListener> connectionListeners;
    private ConnectionState connectionState;
    private ErrorCallback errorCallback;

    public DeepstreamClientMock( ErrorCallback errorCallback ) {
        this.connectionListeners = new ArrayList();
        this.errorCallback = errorCallback;
    }

    public DeepstreamClientMock addConnectionChangeListener( ConnectionChangeListener connectionChangeListener ) {
        connectionListeners.add( connectionChangeListener );
        return this;
    }

    public DeepstreamClientMock removeConnectionChangeListener(ConnectionChangeListener connectionChangeListener) {
        return null;
    }

    public ConnectionState getConnectionState() {
        return this.connectionState;
    }

    public String getUid() {
        return "1";
    }

    public void setConnectionState( ConnectionState state ) {
        this.connectionState = state;
        for ( ConnectionChangeListener listener : this.connectionListeners ) {
            listener.connectionStateChanged( state );
        }
    }

    @Override
    public void onError(Topic topic, Event event, String message) {
        this.errorCallback.onError( topic, event, message );
    }
}
