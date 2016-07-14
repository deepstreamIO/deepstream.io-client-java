package io.deepstream;

import com.google.gson.JsonObject;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.io.FileInputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Properties;
import java.util.UUID;

public class DeepstreamClient implements IDeepstreamClient {

    private String uuid;
    private Connection connection;
    public EventHandler event;
    public RpcHandler rpc;
    public Properties config;

    public DeepstreamClient( final String url, Properties options ) throws URISyntaxException, IOException {
        this.config = getConfig( options );
        this.connection = new Connection( url, this.config, this );
        this.event = new EventHandler( config, this.connection, this );
        this.rpc = new RpcHandler( config, this.connection, this );
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

    public String getUid() {
        if( uuid == null ) {
            uuid = UUID.randomUUID().toString();
            return uuid;
        }
        return uuid;
    }

    public void onError(Topic topic, Event event, String msg) throws DeepstreamException {
        String errMsg;

        /*
         * Help to diagnose the problem quicker by checking for
         * some mon problems
         */
        if( event.equals( Event.ACK_TIMEOUT ) || event.equals( Event.RESPONSE_TIMEOUT ) ) {
            if( getConnectionState().equals( ConnectionState.AWAITING_AUTHENTICATION ) ) {
                errMsg = "Your message timed out because you\'re not authenticated. Have you called login()?";
                onError( Topic.ERROR, Event.NOT_AUTHENTICATED, errMsg );
                return;
            }
        }

        errMsg = msg;
        System.out.println( "--- You can catch all deepstream errors by subscribing to the error event ---" );
        throw new DeepstreamException( topic, event, errMsg );
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
