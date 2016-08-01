package io.deepstream;

import com.google.gson.JsonElement;
import io.deepstream.constants.ConnectionState;

import java.util.ArrayList;

public class DeepstreamClientMock extends DeepstreamClientAbstract {

    private ArrayList<ConnectionStateListener> connectionListeners;
    private ConnectionState connectionState;

    public DeepstreamClientMock() {
        this.connectionListeners = new ArrayList<>();
    }

    public DeepstreamClientMock addConnectionChangeListener( ConnectionStateListener connectionStateListener) {
        connectionListeners.add(connectionStateListener);
        return this;
    }

    public DeepstreamClientMock removeConnectionChangeListener(ConnectionStateListener connectionStateListener) {
        return null;
    }

    public ConnectionState getConnectionState() {
        return this.connectionState;
    }

    @Override
    DeepstreamClientAbstract login(JsonElement data) throws DeepstreamLoginException {
        return this;
    }

    @Override
    DeepstreamClientAbstract login(JsonElement data, LoginCallback loginCallback) throws DeepstreamLoginException {
        return this;
    }

    @Override
    DeepstreamClientAbstract close() {
        return this;
    }

    public String getUid() {
        return "1";
    }

    public void setConnectionState( ConnectionState state ) {
        this.connectionState = state;
        for ( ConnectionStateListener listener : this.connectionListeners ) {
            listener.connectionStateChanged( state );
        }
    }
}
