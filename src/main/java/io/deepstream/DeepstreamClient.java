package io.deepstream;

import com.google.gson.JsonObject;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;
import io.deepstream.event.EventHandler;
import io.deepstream.message.Connection;

import java.io.FileInputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Properties;

public class DeepstreamClient implements IDeepstreamClient {

    private Connection connection;
    public EventHandler event;
    public Properties config;

    public DeepstreamClient( final String url, Properties options ) throws URISyntaxException, IOException {
        this.config = getConfig( options );
        this.connection = new Connection( url, this.config, this );
        this.event = new EventHandler( options, this.connection, this );
    }

    public DeepstreamClient( final String url ) throws URISyntaxException, IOException {
        this( url, new Properties() );
    }

    public DeepstreamClient login( JsonObject data ) throws DeepstreamLoginException {
        this.connection.authenticate( data, null );
        return this;
    }

    public DeepstreamClient login( JsonObject data, LoginCallback loginCallback ) throws DeepstreamLoginException {
        this.connection.authenticate( data, loginCallback );
        return this;
    }

    public DeepstreamClient close() {
        this.connection.close();
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

    public void onError(Topic topic, Event event, String message) throws DeepstreamException {
        System.out.println( "--- You can catch all deepstream errors by subscribing to the error event ---" );
        throw new DeepstreamException( topic, event, message );
    }

    private Properties getConfig( Properties properties ) throws IOException {
        Properties config = new Properties();
        FileInputStream in = new FileInputStream( "DefaultConfig.properties" );
        config.load( in );
        config.putAll( properties );
        in.close();
        return config;
    }
}
