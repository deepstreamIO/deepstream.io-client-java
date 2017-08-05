package io.deepstream;

import com.google.gson.JsonElement;
import com.google.j2objc.annotations.ObjectiveCName;

import java.net.URISyntaxException;
import java.util.Date;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

/**
 * The main entry point for a DeepstreamClient. You can create a client directly using the constructors or use the
 * {@link DeepstreamFactory#getClient()}, {@link DeepstreamFactory#getClient(String)} or
 * {@link DeepstreamFactory#getClient(String, Properties)} to create one for you and hold them for future reference.
 */
public class DeepstreamClient extends DeepstreamClientAbstract {
    private ExecutorService executor;

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
    @ObjectiveCName("init:options:")
    public DeepstreamClient(final String url, Map options) throws URISyntaxException, InvalidDeepstreamConfig {
        this(url, new DeepstreamConfig(options));
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
     * @param endpointFactory An EndpointFactory that returns an Endpoint
     * @throws URISyntaxException Thrown if the url in incorrect
     */
    @ObjectiveCName("init:endpointFactory:")
    public DeepstreamClient(final String url, EndpointFactory endpointFactory) throws URISyntaxException, InvalidDeepstreamConfig{
        this(url, new DeepstreamConfig(), endpointFactory);
    }

    /**
     * deepstream.io java client
     * @param url URL to connect to. The protocol can be omited, e.g. <host>:<port>
     * @param endpointFactory An EndpointFactory that returns an Endpoint
     * @throws URISyntaxException Thrown if the url in incorrect
     */
    @ObjectiveCName("init:options:endpointFactory:")
    public DeepstreamClient(final String url, Properties options, EndpointFactory endpointFactory) throws URISyntaxException, InvalidDeepstreamConfig {
        this(url, new DeepstreamConfig(options), endpointFactory);
    }

    /**
     * deepstream.io java client
     * @param url URL to connect to. The protocol can be omited, e.g. <host>:<port>
     * @param endpointFactory An EndpointFactory that returns an Endpoint
     * @throws URISyntaxException Thrown if the url in incorrect
     */
    @ObjectiveCName("init:properties:endpointFactory:")
    public DeepstreamClient(final String url, Map options, EndpointFactory endpointFactory) throws URISyntaxException, InvalidDeepstreamConfig {
        this(url, new DeepstreamConfig(options), endpointFactory);
    }

    /**
     * deepstream.io java client
     * @param url URL to connect to. The protocol can be omited, e.g. <host>:<port>
     * @param deepstreamConfig A map of options that extend the ones specified in DefaultConfig.properties
     * @throws URISyntaxException Thrown if the url in incorrect
     */
    @ObjectiveCName("init:deepstreamConfig:")
    private DeepstreamClient(final String url, DeepstreamConfig deepstreamConfig) throws URISyntaxException {
        this(url, deepstreamConfig, new JavaEndpointFactory());
    }

    /**
     * deepstream.io java client
     * @param url URL to connect to. The protocol can be omited, e.g. <host>:<port>
     * @param deepstreamConfig A map of options that extend the ones specified in DefaultConfig.properties
     * @param endpointFactory An EndpointFactory that returns an Endpoint
     * @throws URISyntaxException Thrown if the url in incorrect
     */
    @ObjectiveCName("init:deepstreamConfig:endpointFactory:")
    public DeepstreamClient(final String url, DeepstreamConfig deepstreamConfig, EndpointFactory endpointFactory) throws URISyntaxException {
        this(url, deepstreamConfig, endpointFactory, false);
    }

    /**
     * deepstream.io java client
     * @param url URL to connect to. The protocol can be omited, e.g. <host>:<port>
     * @param deepstreamConfig A map of options that extend the ones specified in DefaultConfig.properties
     * @param endpointFactory An EndpointFactory that returns an Endpoint
     * @param networkAvailable indicates whether network is available or not
     * @throws URISyntaxException Thrown if the url in incorrect
     */
    public DeepstreamClient(final String url, DeepstreamConfig deepstreamConfig, EndpointFactory endpointFactory, boolean networkAvailable) throws URISyntaxException {
        this.executor = Executors.newSingleThreadExecutor();
        this.connection = new Connection(url, deepstreamConfig, this, endpointFactory, networkAvailable);
        this.event = new EventHandler(deepstreamConfig, this.connection, this);
        this.rpc = new RpcHandler(deepstreamConfig, this.connection, this, this.executor);
        this.record = new RecordHandler(deepstreamConfig, this.connection, this, this.executor);
        this.presence = new PresenceHandler(deepstreamConfig, this.connection, this, this.executor);
    }

    /**
     * Gets the {@link RecordHandler} used for data-sync in deepstream. Through records and lists {@link RecordHandler#getRecord(String)},
     * {@link RecordHandler#getList(String)}, as well as provider functionality such as {@link RecordHandler#listen(String, ListenListener)}
     * and single requests like {@link RecordHandler#snapshot(String)}
     *
     * @return the {@link RecordHandler}
     */
    public RecordHandler getRecordHandler() {
        return this.record;
    }

    /**
     * Gets the {@link EventHandler} used for events in deepstream. Through subscribing and emitting events
     * {@link EventHandler#subscribe(String, EventListener)}, {@link EventHandler#emit(String)}, as well as provider
     * functionality such as {@link EventHandler#listen(String, ListenListener)}
     *
     * @return the {@link EventHandler}
     */
    public EventHandler getEventHandler() {
        return this.event;
    }

    /**
     * Gets the {@link RpcHandler} used for rpcs in deepstream. Used for both requesting them via {@link RpcHandler#make(String, Object)} and
     * providing them via {@link RpcHandler#provide(String, RpcRequestedListener)}
     */
    public RpcHandler getRpcHandler() {
        return this.rpc;
    }

    /**
     * Gets the {@link PresenceHandler} for presence functionality in deepstream. Both querying for clients via {@link PresenceHandler#getAll()}
     * and subscribing/unsubscribing to login events via {@link PresenceHandler#subscribe(PresenceEventListener)} and
     * {@link PresenceHandler#unsubscribe(PresenceEventListener)}
     */
    public PresenceHandler getPresenceHandler() {
        return this.presence;
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
    @ObjectiveCName("login")
    public LoginResult login(){
        return this.login(null);
    }

    /**
     * Synchronously sends authentication parameters to the client to fully open
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
     * This will block your calling thread
     *
     * @param authParams JSON.serializable authentication data
     * @return The login result
     */
    @ObjectiveCName("login:")
    public LoginResult login(final JsonElement authParams){
        try {
            return this.loginAsync(authParams, null).get();
        }catch(Exception e){
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Asynchronously sends authentication parameters to the client to fully open
     * the connection and sends callback afterwards.
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
     * This will not block your main thread. However you can block it by calling get().
     *
     * @param authParams JSON.serializable authentication data
     * @param listener Callback to be called after login is successfull, may be null
     * @return The login result
     */
    @ObjectiveCName("loginAsync:")
    public Future<LoginResult> loginAsync(final JsonElement authParams, final LoginResultListener listener) {
        return executor.submit(new Callable<LoginResult>() {
            @Override
            public LoginResult call() throws Exception {
                final CountDownLatch loggedInLatch = new CountDownLatch(1);
                final LoginResult[] loginResult = new LoginResult[1];

                connection.authenticate(authParams, new LoginCallback() {
                    @Override
                    @ObjectiveCName("loginSuccess:")
                    public void loginSuccess(Object userData) {
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
                if(listener != null) {
                    listener.loginCompleted(loginResult[0]);
                }
                return loginResult[0];
            }
        });
    }

    /**
     * Closes the connection to the server.
     * @return The deepstream client
     */
    public DeepstreamClient close() {
        this.executor.shutdown();
        this.connection.close(true);
        this.getAckTimeoutRegistry().close();
        return this;
    }

    /**
     * Add a listener that can be notified via {@link ConnectionStateListener#connectionStateChanged(ConnectionState)}
     * whenever the {@link ConnectionState} changes
     * @param connectionStateListener The listener to add
     * @return The deepstream client
     */
    @ObjectiveCName("addConnectionChangedListener:")
    public DeepstreamClient addConnectionChangeListener( ConnectionStateListener connectionStateListener) {
        this.connection.addConnectionChangeListener(connectionStateListener);
        return this;
    }

    /**
     * Removes a {@link ConnectionStateListener} added via {@link DeepstreamClient#addConnectionChangeListener(ConnectionStateListener)}
     * @param connectionStateListener The listener to remove
     * @return The deepstream client
     */
    @ObjectiveCName("removeConnectionChangedListener:")
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
     * Set global connectivity state.
     * @param  {GlobalConnectivityState} globalConnectivityState Current global connectivity state
     */
    public void setGlobalConnectivityState(GlobalConnectivityState globalConnectivityState){
        this.connection.setGlobalConnectivityState(globalConnectivityState);
    }

    /**
     * Returns a random string. The first block of characters
     * is a timestamp, in order to allow databases to optimize for semi-
     * sequentuel numberings
     * @return A unique id
     */
    public String getUid() {
        String date = Long.toString(new Date().getTime(), 36);
        String random = Long.toString((long) (Math.random() * 100000000000000000L), 36);
        return date + "-" + random;
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
        void loginSuccess(Object userData);

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
