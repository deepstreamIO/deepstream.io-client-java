package io.deepstream;

import com.google.gson.JsonElement;

import java.util.ArrayList;
import java.util.HashMap;

public class DeepstreamClientMock extends DeepstreamClientAbstract {

    private ArrayList<ConnectionStateListener> connectionListeners;
    private ConnectionState connectionState;

    public DeepstreamClientMock() {
        this.connectionListeners = new ArrayList<ConnectionStateListener>();
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
    LoginResult login() {
        return new LoginResult( true, new HashMap());
    }

    @Override
    LoginResult login(JsonElement data) {
        return new LoginResult( true, new HashMap());
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
