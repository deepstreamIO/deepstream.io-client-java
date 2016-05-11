package io.deepstream;

import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;

import io.deepstream.constants.ConnectionState;
import io.deepstream.event.EventHandler;
import io.deepstream.message.Connection;
import org.json.JSONObject;

public class DeepstreamClient {

    private Connection connection;
    public EventHandler event;

    public DeepstreamClient( final String url, Map options ) throws URISyntaxException {
        this.connection = new Connection( url, options, this );
        this.event = new EventHandler( options, this.connection );
    }

    public DeepstreamClient( final String url ) throws URISyntaxException {
        this( url, new HashMap() );
    }

    public DeepstreamClient login( JSONObject data ) {
        this.connection.authenticate( data, null );
        return this;
    }

    public DeepstreamClient login( JSONObject data, LoginCallback loginCallback ) {
        this.connection.authenticate( data, loginCallback );
        return this;
    }

    public DeepstreamClient close() {
        return this;
    }

    public DeepstreamClient addConnectionChangeListener( ConnectionChangeListener connectionChangeListener ) {
        this.connection.addConnectionChangeListener( connectionChangeListener );
        return this;
    }

    public DeepstreamClient removeConnectionChangeListener( ConnectionChangeListener connectionChangeListener ) {
        this.connection.removeConnectionChangeListener( connectionChangeListener );
        return this;
    }

    public ConnectionState getConnectionState() {
        return this.connection.getConnectionState();
    }

}
