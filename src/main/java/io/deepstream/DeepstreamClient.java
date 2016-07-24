package io.deepstream;

import com.google.gson.JsonElement;
import io.deepstream.constants.ConnectionState;

import java.io.FileInputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Properties;
import java.util.UUID;

/**
 *
 */
public class DeepstreamClient extends IDeepstreamClient {

    private String uuid;
    private final Connection connection;
    private final Properties config;

    public final RecordHandler record;
    public final EventHandler event;
    public final RpcHandler rpc;

    /**
     * deepstream.io java client
     * @param url
     * @param options
     * @throws URISyntaxException
     * @throws IOException
     */
    public DeepstreamClient( final String url, Properties options ) throws URISyntaxException, IOException {
        this.config = getConfig( options );
        this.connection = new Connection( url, this.config, this );
        this.event = new EventHandler( config, this.connection, this );
        this.rpc = new RpcHandler( config, this.connection, this );
        this.record = new RecordHandler( config, this.connection, this );
    }

    /**
     * deepstream.io javascript client
     * @param url
     * @throws URISyntaxException
     * @throws IOException
     */
    public DeepstreamClient( final String url ) throws URISyntaxException, IOException {
        this( url, new Properties() );
    }

    /**
     * Send authentication parameters to the client to fully open
     * the connection.
     *
     * Please note: Authentication parameters are send over an already established
     * connection, rather than appended to the server URL. This means the parameters
     * will be encrypted when used with a WSS / HTTPS connection. If the deepstream server
     * on the other side has message logging enabled it will however be written to the logs in
     * plain text. If additional security is a requirement it might therefor make sense to hash
     * the password on the client.
     *
     * If the connection is not yet established the authentication parameter will be
     * stored and send once it becomes available
     *
     * authParams can be any JSON serializable data structure and its up for the
     * permission handler on the server to make sense of them, although something
     * like { username: 'someName', password: 'somePass' } will probably make the most sense.
     *
     * login can be called multiple times until either the connection is authenticated or
     * forcefully closed by the server since its maxAuthAttempts threshold has been exceeded
     *
     * @param authParams
     * @return
     * @throws DeepstreamLoginException
     */
    public DeepstreamClient login( JsonElement authParams ) throws DeepstreamLoginException {
        this.connection.authenticate( authParams, null );
        return this;
    }

    /**
     * @param authParams
     * @param loginCallback
     * @return
     * @throws DeepstreamLoginException
     */
    public DeepstreamClient login( JsonElement authParams, LoginCallback loginCallback ) throws DeepstreamLoginException {
        this.connection.authenticate( authParams, loginCallback );
        return this;
    }

    /**
     * Closes the connection to the server.
     * @return
     */
    public DeepstreamClient close() {
        this.connection.close();
        return this;
    }

    /**
     *
     * @param connectionChangeListener
     * @return
     */
    public DeepstreamClient addConnectionChangeListener( ConnectionChangeListener connectionChangeListener ) {
        this.connection.addConnectionChangeListener( connectionChangeListener );
        return this;
    }

    /**
     *
     * @param connectionChangeListener
     * @return
     */
    public DeepstreamClient removeConnectionChangeListener( ConnectionChangeListener connectionChangeListener ) {
        this.connection.removeConnectionChangeListener( connectionChangeListener );
        return this;
    }

    /**
     * Returns the current state of the connection.
     * @return
     */
    public ConnectionState getConnectionState() {
        return this.connection.getConnectionState();
    }

    /**
     * Returns a random string. The first block of characters
     * is a timestamp, in order to allow databases to optimize for semi-
     * sequentuel numberings
     * @return
     */
    public String getUid() {
        if( uuid == null ) {
            uuid = UUID.randomUUID().toString();
            return uuid;
        }
        return uuid;
    }

    /**
     * Creates a new options map by extending default
     * options with the passed in options
     * @param properties
     * @return
     * @throws IOException
     */
    private Properties getConfig( Properties properties ) throws IOException {
        Properties config = new Properties();
        FileInputStream in = new FileInputStream( "DefaultConfig.properties" );
        config.load( in );
        config.putAll( properties );
        in.close();
        return config;
    }
}
