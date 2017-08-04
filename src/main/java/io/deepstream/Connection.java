package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Establishes a connection to a deepstream server, either
 * using TCP or via a WebSocket
 */
class Connection implements IConnection {

    private final DeepstreamClient client;
    private final String originalUrl;
    private final ArrayList<ConnectionStateListener> connectStateListeners;
    private final DeepstreamConfig options;
    private final EndpointFactory endpointFactory;
    private Endpoint endpoint;
    private boolean tooManyAuthAttempts;
    private boolean challengeDenied;
    private boolean deliberateClose;
    private boolean redirecting;
    private Timer reconnectTimeout;
    private int reconnectionAttempt;
    private StringBuilder messageBuffer;
    private String url;
    private ConnectionState connectionState;
    private GlobalConnectivityState globalConnectivityState;
    private DeepstreamClient.LoginCallback loginCallback;
    private JsonElement authParameters;

    private ExecutorService rpcThread;
    private ExecutorService recordThread;
    private ExecutorService eventThread;
    private ExecutorService presenceThread;

    /**
     * Creates an endpoint and passed it to {@link Connection#Connection(String, DeepstreamConfig, DeepstreamClient, EndpointFactory, Endpoint)}
     *
     * @see Connection#Connection(String, DeepstreamConfig, DeepstreamClient, EndpointFactory, Endpoint)
     *
     * @param url The endpoint url
     * @param options The options used to initialise the deepstream client
     * @param endpointFactory The factory to create endpoints
     * @param client The deepstream client
     * @throws URISyntaxException An exception if an invalid url is passed in
     */
    Connection(final String url, final DeepstreamConfig options, DeepstreamClient client, EndpointFactory endpointFactory) throws URISyntaxException {
        this( url, options, client, endpointFactory, null );
        this.endpoint = createEndpoint();
    }

    /**
     * Creates a connection, that is responsible for handling all the connection related logic related to state
     * and messages
     * @param url The endpoint url* @param options The options used to initialise the deepstream client
     * @param client The deepstream client
     * @param endpoint The endpoint, whether Websocket, mock or anything else
     */
    Connection(final String url, final DeepstreamConfig options, DeepstreamClient client, Endpoint endpoint) {
        this( url, options, client, null, endpoint);
    }


    public Connection(String url, DeepstreamConfig options, DeepstreamClient client, EndpointFactory endpointFactory, Endpoint endpoint) {
        this.client = client;
        this.connectStateListeners = new ArrayList<>();
        this.originalUrl = url;
        this.url = url;
        this.connectionState = ConnectionState.CLOSED;
        this.globalConnectivityState = GlobalConnectivityState.DISCONNECTED;
        this.messageBuffer = new StringBuilder();
        this.tooManyAuthAttempts = false;
        this.challengeDenied = false;
        this.deliberateClose = false;
        this.redirecting = false;
        this.reconnectTimeout = null;
        this.reconnectionAttempt = 0;
        this.options = options;
        this.endpoint = endpoint;
        this.endpointFactory = endpointFactory;

        this.recordThread = Executors.newSingleThreadExecutor();
        this.eventThread = Executors.newSingleThreadExecutor();
        this.rpcThread = Executors.newSingleThreadExecutor();
        this.presenceThread = Executors.newSingleThreadExecutor();
    }

    /**
     * Authenticate the user connection
     * @param authParameters The authentication parameters to send to deepstream
     * @param loginCallback The callback for a successful / unsuccessful login attempt
     * connection
     */
    @ObjectiveCName("authenticate:loginCallback:")
    void authenticate(JsonElement authParameters, DeepstreamClient.LoginCallback loginCallback) {
        this.loginCallback = loginCallback;

        if(authParameters != null) {
            this.authParameters = authParameters;
        } else {
            this.authParameters = new JsonObject();
        }

        if( this.tooManyAuthAttempts || this.challengeDenied ) {
            this.client.onError( Topic.ERROR, Event.IS_CLOSED, "The client\'s connection was closed" );
            this.loginCallback.loginFailed(Event.IS_CLOSED, "The client\'s connection was closed");
            return;
        }

        if( this.connectionState == ConnectionState.AWAITING_AUTHENTICATION ) {
            this.sendAuthMessage();
        }
    }

    @Override
    @ObjectiveCName("send:")
    public void send( String message ) {
        if( this.connectionState != ConnectionState.OPEN ) {
            this.messageBuffer.append( message );
        } else {
            this.endpoint.send( message );
        }
    }

    @Override
    @ObjectiveCName("sendMsg:action:data:")
    public void sendMsg( Topic topic, Actions action, String[] data ) {
        this.send( MessageBuilder.getMsg( topic, action, data ) );
    }

    /**
     *
     */
    private void sendAuthMessage() {
        setState( ConnectionState.AUTHENTICATING );
        String authMessage;
        if( this.authParameters == null ) {
            authMessage = MessageBuilder.getMsg(Topic.AUTH, Actions.REQUEST, new JsonObject().toString());
        } else {
            authMessage = MessageBuilder.getMsg(Topic.AUTH, Actions.REQUEST, authParameters.toString());
        }
        this.endpoint.send( authMessage );
    }

    void addConnectionChangeListener( ConnectionStateListener connectionStateListener) {
        this.connectStateListeners.add(connectionStateListener);
    }

    void removeConnectionChangeListener( ConnectionStateListener connectionStateListener) {
        this.connectStateListeners.remove(connectionStateListener);
    }

    ConnectionState getConnectionState() {
        return this.connectionState;
    }

    public void close(boolean forceClose) {
        this.deliberateClose = true;

        if(forceClose && endpoint != null) {
            endpoint.forceClose();
        } else if(this.endpoint != null) {
            endpoint.close();
            endpoint = null;
        }
        if( this.reconnectTimeout != null ) {
            this.reconnectTimeout.cancel();
            this.reconnectTimeout = null;
        }

        this.recordThread.shutdown();
        this.eventThread.shutdown();
        this.rpcThread.shutdown();
        this.presenceThread.shutdown();
    }

    void onOpen() {
        this.setState( ConnectionState.AWAITING_CONNECTION );
    }

    @ObjectiveCName("onError:")
    void onError(final String error ) {
        this.setState( ConnectionState.ERROR );

        /*
         * If the implementation isn't listening on the error event this will throw
         * an error. So let's defer it to allow the reconnection to kick in.
         */
        Timer timer = new Timer();
        timer.schedule(new TimerTask() {
            public void run() {
                client.onError( null, Event.CONNECTION_ERROR, error);
            }
        }, 1000);
    }

    @ObjectiveCName("onMessage:")
    void onMessage(String rawMessage) {
        List<Message> parsedMessages = MessageParser.parse( rawMessage, this.client );
        for (final Message message : parsedMessages) {
            if (message.topic == Topic.CONNECTION) {
                handleConnectionResponse(message);
            } else if (message.topic == Topic.AUTH) {
                handleAuthResponse(message);
            } else if (message.topic == Topic.EVENT) {
                this.eventThread.execute(new Runnable() {
                    @Override
                    public void run() {
                        client.event.handle(message);
                    }
                });
            } else if (message.topic == Topic.RPC) {
                this.rpcThread.execute(new Runnable() {
                    @Override
                    public void run() {
                        client.rpc.handle(message);
                    }
                });
            } else if ( message.topic == Topic.RECORD ) {
                this.recordThread.execute(new Runnable() {
                    @Override
                    public void run() {
                        client.record.handle(message);
                    }
                });
            } else if ( message.topic == Topic.PRESENCE ) {
                this.presenceThread.execute(new Runnable() {
                    @Override
                    public void run() {
                        client.presence.handle( message );
                    }
                });
            } else {
                this.client.onError(Topic.ERROR, Event.UNSOLICITED_MESSAGE, message.action.toString());
            }
        }
    }

    void onClose() throws URISyntaxException {
        if( this.redirecting ) {
            this.redirecting = false;
            this.endpoint = this.createEndpoint();
        }
        else if( this.deliberateClose ) {
            this.setState( ConnectionState.CLOSED );
        }
        else {
            if(!this.originalUrl.equals(this.url)) {
                this.url = this.originalUrl;
                this.endpoint = this.createEndpoint();
                return;
            }
            this.tryReconnect();
        }
    }

    @ObjectiveCName("handleConnectionResponse:")
    private void handleConnectionResponse( Message message ) {
        if( message.action == Actions.PING ) {
            this.endpoint.send( MessageBuilder.getMsg( Topic.CONNECTION, Actions.PONG ) );
        }
        else if( message.action == Actions.ACK ) {
            this.setState( ConnectionState.AWAITING_AUTHENTICATION );
        }
        else if( message.action == Actions.CHALLENGE ) {
            this.setState( ConnectionState.CHALLENGING );
            this.endpoint.send( MessageBuilder.getMsg( Topic.CONNECTION, Actions.CHALLENGE_RESPONSE,  this.originalUrl ) );
        }
        else if( message.action == Actions.REJECTION ) {
            this.challengeDenied = true;
            this.close(false);
        }
        else if( message.action == Actions.REDIRECT ) {
            this.url = message.data[ 0 ];
            this.redirecting = true;
            this.endpoint.close();
            this.endpoint = null;
        }
    }

    @ObjectiveCName("handleAuthResponse:")
    private void handleAuthResponse( Message message ) {
        if( message.action == Actions.ERROR ) {
            if( message.data[0].equals( Event.TOO_MANY_AUTH_ATTEMPTS.name() ) ) {
                this.deliberateClose = true;
                this.tooManyAuthAttempts = true;
            } else {
                this.authParameters = null;
                this.setState( ConnectionState.AWAITING_AUTHENTICATION );
            }

            if( this.loginCallback != null ) {
                this.loginCallback.loginFailed(
                        Event.getEvent( message.data[ 0 ] ),
                        MessageParser.convertTyped(message.data[1], this.client, this.options.getJsonParser())
                );
            }
        }
        else if( message.action == Actions.ACK ) {
            this.setState( ConnectionState.OPEN );

            if( this.messageBuffer.length() > 0 ) {
                this.endpoint.send( this.messageBuffer.toString() );
                this.messageBuffer = new StringBuilder();
            }

            if( this.loginCallback != null ) {
                try {
                    Object data = MessageParser.convertTyped(message.data[0], this.client, this.options.getJsonParser());
                    this.loginCallback.loginSuccess(data);
                } catch (IndexOutOfBoundsException e) {
                    this.loginCallback.loginSuccess(null);
                }
            }
        }
    }

    @ObjectiveCName("setState:")
    private void setState( ConnectionState connectionState ) {
        this.connectionState = connectionState;

        for (ConnectionStateListener connectStateListener : this.connectStateListeners) {
            connectStateListener.connectionStateChanged(connectionState);
        }

        if( connectionState == ConnectionState.AWAITING_AUTHENTICATION && this.authParameters != null ) {
            this.sendAuthMessage();
        }
    }

    /**
     * Set global connectivity state.
     * @param  {GlobalConnectivityState} globalConnectivityState Current global connectivity state
     */
    protected void setGlobalConnectivityState(GlobalConnectivityState globalConnectivityState){
        this.globalConnectivityState = globalConnectivityState;
        if(globalConnectivityState == GlobalConnectivityState.CONNECTED){
            if(this.connectionState == ConnectionState.CLOSED || this.connectionState == ConnectionState.ERROR) {
                tryReconnect();
            }
        }else{
            if(this.reconnectTimeout != null){
                this.reconnectTimeout.cancel();
            }
            this.reconnectTimeout = null;
            this.reconnectionAttempt = 0;
            this.endpoint.forceClose();
            this.setState(ConnectionState.CLOSED);
        }
    }

    /**
     * Take the url passed when creating the client and ensure the correct
     * protocol is provided
     * @param  {String} url Url passed in by client
     * @param  {String} defaultPath Default path to concatenate if one doest not exist
     * @return {String} Url with supported protocol
     */
    private URI parseUri(String url, String defaultPath) throws URISyntaxException {
        if (url.startsWith("http:") || url.startsWith("https:")) {
            throw new URISyntaxException(url, "HTTP/HTTPS is not supported, please use ws or wss instead");
        }
        if (url.startsWith("//")) {
            url = "ws:" + url;
        } else if (!url.startsWith("ws:") && !url.startsWith("wss:")) {
            url = "ws://" + url;
        }
        URI uri = new URI(url);
        if (uri.getPath().isEmpty()) {
            uri = uri.resolve(defaultPath);
        }
        return uri;
    }

    private Endpoint createEndpoint() throws URISyntaxException {
        URI uri = parseUri(url, this.options.getPath());
        Endpoint endpoint = this.endpointFactory.createEndpoint(uri, this);
        endpoint.open();
        return endpoint;
    }

    private void tryReconnect() {
        if( this.reconnectTimeout != null ) {
            return;
        }

        int maxReconnectAttempts = options.getMaxReconnectAttempts();
        int reconnectIntervalIncrement = options.getReconnectIntervalIncrement();
        int maxReconnectInterval = options.getMaxReconnectInterval();

        if( this.reconnectionAttempt < maxReconnectAttempts ) {
            if(this.globalConnectivityState == GlobalConnectivityState.CONNECTED) {
                this.setState(ConnectionState.RECONNECTING);
                this.reconnectTimeout = new Timer();
                this.reconnectTimeout.schedule(new TimerTask() {
                    public void run() {
                        tryOpen();
                    }
                }, Math.min(
                        reconnectIntervalIncrement * this.reconnectionAttempt,
                        maxReconnectInterval
                ));
                this.reconnectionAttempt++;
            }

        } else {
            this.clearReconnect();
            this.close(true);
        }
    }

    private void tryOpen() {
        this.reconnectTimeout.cancel();
        this.reconnectTimeout = null;
        this.endpoint.open();
    }

    private void clearReconnect() {
        this.reconnectTimeout = null;
        this.reconnectionAttempt = 0;
    }
}
