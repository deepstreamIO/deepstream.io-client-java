package io.deepstream;

import com.google.gson.JsonElement;

import com.google.j2objc.annotations.ObjectiveCName;

import java.net.URISyntaxException;
import java.util.Map;
import java.util.Properties;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;

/**
 * The main entry point for a DeepstreamClient. You can create a client directly using the constructors or use the
 * {@link DeepstreamFactory#getClient()}, {@link DeepstreamFactory#getClient(String)} or
 * {@link DeepstreamFactory#getClient(String, Properties)} to create one for you and hold them for future reference.
 */
public class DeepstreamClient extends DeepstreamClientAbstract {

    /**
     * The getters for data-sync, such as {@link RecordHandler#getRecord(String)},
     * {@link RecordHandler#getList(String)}, provider functionality such as {@link RecordHandler#listen(String, ListenListener)}
     * and single requests like {@link RecordHandler#snapshot(String)}
     */
    public final RecordHandler record;
    /**
     * The entry point for events, such as {@link EventHandler#subscribe(String, EventListener)},
     * {@link EventHandler#emit(String)} and provider functionality such as {@link EventHandler#listen(String, ListenListener)}
     */
    public final EventHandler event;
    /**
     * The entry point for rpcs, both requesting them via {@link RpcHandler#make(String, Object)} and
     * providing them via {@link RpcHandler#provide(String, RpcRequestedListener)}
     */
    public final RpcHandler rpc;
    /**
     * The entry point for presence, both querying for clients via {@link PresenceHandler#getAll()}
     * and subscribing/unsubscribing to login events via {@link PresenceHandler#subscribe(PresenceEventListener)} and
     * {@link PresenceHandler#unsubscribe(PresenceEventListener)}
     */
    public final PresenceHandler presence;
    private final Connection connection;
    private String uuid;

    /**
     * deepstream.io javascript client, defaults to using default properties
     * {@link DeepstreamClient#DeepstreamClient(String, Properties)}
     *
     * @throws URISyntaxException Thrown if the url in incorrect
     */
    @ObjectiveCName("init:")
    public DeepstreamClient(final String url) throws URISyntaxException {
        this(url, new DeepstreamConfig());
    }

    /**
     * deepstream.io javascript client
     *
     * @throws URISyntaxException Thrown if the url in incorrect
     */
    @ObjectiveCName("init:properties:")
    public DeepstreamClient(final String url, Properties options) throws URISyntaxException, InvalidDeepstreamConfig {
        this(url, new DeepstreamConfig(options));
    }

    /**
     * deepstream.io java client
     * @param url URL to connect to. The protocol can be omited, e.g. <host>:<port>
     * @param deepstreamConfig A map of options that extend the ones specified in DefaultConfig.properties
     * @throws URISyntaxException Thrown if the url in incorrect
     */
    @ObjectiveCName("init:deepstreamConfig:")
    private DeepstreamClient(final String url, DeepstreamConfig deepstreamConfig) throws URISyntaxException {
        this.connection = new Connection(url, deepstreamConfig, this);
        this.event = new EventHandler(deepstreamConfig, this.connection, this);
        this.rpc = new RpcHandler(deepstreamConfig, this.connection, this);
        this.record = new RecordHandler(deepstreamConfig, this.connection, this);
        this.presence = new PresenceHandler(deepstreamConfig, this.connection, this);
    }

    /**
     * Adds a {@link DeepstreamRuntimeErrorHandler} that will catch all RuntimeErrors such as AckTimeouts and allow
     * the user to gracefully handle them.
     *
     * @param deepstreamRuntimeErrorHandler The listener to set
     */
    @ObjectiveCName("setRuntimeErrorHandler:")
    public void setRuntimeErrorHandler( DeepstreamRuntimeErrorHandler deepstreamRuntimeErrorHandler )  {
        super.setRuntimeErrorHandler( deepstreamRuntimeErrorHandler );
    }

    /**
     * @see DeepstreamClient#login(JsonElement)
     *
     * Does not call the login callback, used mainly for anonymous logins where your guaranteed login
     *
     * @return The login result
     */
    public LoginResult login() {
        return this.login(null);
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
     * @param authParams JSON.serializable authentication data
     * @return The login result
     */
    @ObjectiveCName("login:")
    public LoginResult login(JsonElement authParams) {
        final CountDownLatch loggedInLatch = new CountDownLatch(1);
        final LoginResult[] loginResult = new LoginResult[1];

        this.connection.authenticate(authParams, new LoginCallback() {
            @Override
            @ObjectiveCName("loginSuccess:")
            public void loginSuccess(Map userData) {
                loginResult[0] = new LoginResult(true, userData);
                loggedInLatch.countDown();
            }

            @Override
            @ObjectiveCName("loginFailed:data:")
            public void loginFailed(Event errorEvent, Object data) {
                loginResult[0] = new LoginResult(false, errorEvent, data);
                loggedInLatch.countDown();
            }
        });

        try {
            loggedInLatch.await();
        } catch (InterruptedException e) {
            loginResult[0] = new LoginResult(false, null, "An issue occured during login");
        }

        return loginResult[0];
    }

    /**
     * Closes the connection to the server.
     * @return The deepstream client
     */
    public DeepstreamClient close() {
        this.connection.close();
        return this;
    }

    /**
     * Add a listener that can be notified via {@link ConnectionStateListener#connectionStateChanged(ConnectionState)}
     * whenever the {@link ConnectionState} changes
     * @param connectionStateListener The listener to add
     * @return The deepstream client
     */
    @ObjectiveCName("addConnectionChangeListener:")
    public DeepstreamClient addConnectionChangeListener( ConnectionStateListener connectionStateListener) {
        this.connection.addConnectionChangeListener(connectionStateListener);
        return this;
    }

    /**
     * Removes a {@link ConnectionStateListener} added via {@link DeepstreamClient#addConnectionChangeListener(ConnectionStateListener)}
     * @param connectionStateListener The listener to remove
     * @return The deepstream client
     */
    @ObjectiveCName("removeConnectionChangeListener:")
    public DeepstreamClient removeConnectionChangeListener( ConnectionStateListener connectionStateListener) {
        this.connection.removeConnectionChangeListener(connectionStateListener);
        return this;
    }

    /**
     * Returns the current state of the connection.
     * @return The connection state
     */
    public ConnectionState getConnectionState() {
        return this.connection.getConnectionState();
    }

    /**
     * Returns a random string. The first block of characters
     * is a timestamp, in order to allow databases to optimize for semi-
     * sequentuel numberings
     * @return A unique id
     */
    public String getUid() {
        if( uuid == null ) {
            uuid = UUID.randomUUID().toString();
            return uuid;
        }
        return uuid;
    }

    /**
     * A callback that notifies the user if the login process was completed successfully or not, and contains optional data
     * received from the server associated to the user
     */
    interface LoginCallback {

        /**
         * Called when {@link DeepstreamClient#login(JsonElement)} is successful
         *
         * @param userData Optional data that is specific to the user and returned on succesfuly authentication
         */
        @ObjectiveCName("loginSuccess:")
        void loginSuccess(Map userData);

        /**
         * Called when {@link DeepstreamClient#login(JsonElement)} is unsuccessful
         *
         * @param errorEvent error event
         * @param data       Contains data associated to the failed login, such as the reason
         */
        @ObjectiveCName("loginFailed:data:")
        void loginFailed(Event errorEvent, Object data);
    }
}
