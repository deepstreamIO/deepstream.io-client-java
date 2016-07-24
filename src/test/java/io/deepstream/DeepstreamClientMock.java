package io.deepstream;

import com.google.gson.JsonElement;
import io.deepstream.constants.ConnectionState;

import java.util.ArrayList;

public class DeepstreamClientMock extends IDeepstreamClient {

    private ArrayList<ConnectionChangeListener> connectionListeners;
    private ConnectionState connectionState;

    public DeepstreamClientMock() {
        this.connectionListeners = new ArrayList();
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

    @Override
    IDeepstreamClient login(JsonElement data) throws DeepstreamLoginException {
        return this;
    }

    @Override
    IDeepstreamClient login(JsonElement data, LoginCallback loginCallback) throws DeepstreamLoginException {
        return this;
    }

    @Override
    IDeepstreamClient close() {
        return this;
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
}
