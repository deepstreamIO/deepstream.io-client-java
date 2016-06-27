package io.deepstream.message;

import com.google.gson.JsonObject;
import io.deepstream.*;
import io.deepstream.constants.*;

import java.net.URISyntaxException;
import java.util.*;

public class Connection implements IConnection {

    Endpoint endpoint;

    private DeepstreamClient client;
    private String originalUrl;
    private String url;
    private ConnectionState connectionState;
    private ArrayList<ConnectionChangeListener> connectStateListeners;

    private boolean tooManyAuthAttempts;
    private boolean challengeDenied;
    private boolean deliberateClose;
    private boolean redirecting;
    private Timer reconnectTimeout;
    private int reconnectionAttempt;
    private StringBuilder messageBuffer;

    private LoginCallback loginCallback;
    private JsonObject authParameters;
    private Map options;

    public Connection(final String url, final Map options, DeepstreamClient client ) throws URISyntaxException {
        this( url, options, client, null );
        this.endpoint = createEndpoint();
    }

    Connection(final String url, final Map options, DeepstreamClient client, Endpoint endpoint ) {
        this.client = client;
        this.connectStateListeners = new ArrayList<>();
        this.originalUrl = url;
        this.url = url;
        this.connectionState = ConnectionState.CLOSED;
        this.messageBuffer = new StringBuilder();
        this.tooManyAuthAttempts = false;
        this.challengeDenied = false;
        this.deliberateClose = false;
        this.redirecting = false;
        this.reconnectTimeout = null;
        this.reconnectionAttempt = 0;
        this.options = options;
        this.endpoint = endpoint;
    }

    public void authenticate(JsonObject authParameters, LoginCallback loginCallback ) throws DeepstreamLoginException {
        if( this.tooManyAuthAttempts || this.challengeDenied ) {
            this.client.onError( Topic.ERROR, Event.IS_CLOSED, "The client\'s connection was closed" );
            return;
        }
        this.loginCallback = loginCallback;
        this.authParameters = authParameters;

        if( this.connectionState == ConnectionState.AWAITING_AUTHENTICATION ) {
            this.sendAuthMessage();
        }
    }

    public void send( String message ) {
        if( this.connectionState != ConnectionState.OPEN ) {
            this.messageBuffer.append( message );
            System.out.println( "Buffering " + message );
        } else {
            System.out.println( "Sending " + message );
            this.endpoint.send( message );
        }
    }

    public void sendMsg( Topic topic, Actions action, String[] data ) {
        this.send( MessageBuilder.getMsg( topic, action, data ) );
    }

    private void sendAuthMessage() {
        setState( ConnectionState.AUTHENTICATING );
        String authMessage = MessageBuilder.getMsg( Topic.AUTH, Actions.REQUEST, this.authParameters.toString() );
        this.endpoint.send( authMessage );
    }

    public void addConnectionChangeListener( ConnectionChangeListener connectionChangeListener ) {
        this.connectStateListeners.add( connectionChangeListener );
    }

    public void removeConnectionChangeListener( ConnectionChangeListener connectionChangeListener ) {
        this.connectStateListeners.remove( connectionChangeListener );
    }

    public ConnectionState getConnectionState() {
        return this.connectionState;
    }

    public void close() {
        this.deliberateClose = true;
        if( this.endpoint != null ) {
            endpoint.close();
            endpoint = null;
        }
    }

    void onOpen() {
        this.setState( ConnectionState.AWAITING_CONNECTION );
    }

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

    void onMessage(String rawMessage) {
        List<Message> parsedMessages = MessageParser.parse( rawMessage, this );
        for (Message message : parsedMessages) {
            if (message.topic == Topic.CONNECTION) {
                handleConnectionResponse(message);
            } else if (message.topic == Topic.AUTH) {
                handleAuthResponse(message);
            } else if (message.topic == Topic.EVENT) {
                this.client.event.handle(message);
            } else if (message.topic == Topic.RPC) {
                this.client.rpc.handle(message);
            } else {
                System.out.println("Normal message of type " + message.topic);
            }
        }
    }

    void onClose() throws URISyntaxException {
        if( this.redirecting ) {
            this.redirecting = false;
            this.createEndpoint();
        }
        else if( this.deliberateClose ) {
            this.setState( ConnectionState.CLOSED );
        }
        else {
            if( this.originalUrl.equals( this.url ) == false ) {
                this.url = this.originalUrl;
                this.createEndpoint();
                return;
            }
            this.tryReconnect();
        }
    }

    private void handleConnectionResponse( Message message ) {
        if( message.action == Actions.ACK ) {
            this.setState( ConnectionState.AWAITING_AUTHENTICATION );
            if( this.authParameters != null ) {
                this.sendAuthMessage();
            }
        }
        else if( message.action == Actions.CHALLENGE ) {
            this.setState( ConnectionState.CHALLENGING );
            this.endpoint.send( MessageBuilder.getMsg( Topic.CONNECTION, Actions.CHALLENGE_RESPONSE,  this.originalUrl ) );
        }
        else if( message.action == Actions.REJECTION ) {
            this.challengeDenied = true;
            this.close();
        }
        else if( message.action == Actions.REDIRECT ) {
            this.url = message.data[ 0 ];
            this.redirecting = true;
            this.endpoint.close();
        }
    }

    private void handleAuthResponse( Message message ) {
        if( message.action == Actions.ERROR ) {
            if( message.data[0].equals( Event.TOO_MANY_AUTH_ATTEMPTS.name() ) ) {
                this.tooManyAuthAttempts = true;
            } else {
                this.setState( ConnectionState.AWAITING_AUTHENTICATION );
            }

            if( this.loginCallback != null ) {
                this.loginCallback.loginFailed(
                        Event.getEvent( message.data[ 0 ] ),
                        MessageParser.convertTyped( message.data[ 1 ] )
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
                this.loginCallback.loginSuccess( new HashMap() );
            }
        }
    }

    private void setState( ConnectionState connectionState ) {
        this.connectionState = connectionState;

        if( connectionState == ConnectionState.AWAITING_CONNECTION && this.authParameters != null ) {
            this.sendAuthMessage();
        }

        Iterator listeners = this.connectStateListeners.iterator();
        while( listeners.hasNext() ) {
            ( (ConnectionChangeListener)listeners.next() ).connectionStateChanged( connectionState );
        }
    }

    private Endpoint createEndpoint() throws URISyntaxException {
        Endpoint endpoint = null;

        if( options.get( "endpoint" ).equals( EndpointType.TCP.name() ) ) {
            endpoint = new EndpointTCP( url, options, this );
            this.endpoint = endpoint;
        } else if( options.get( "endpoint" ).equals( EndpointType.ENGINEIO.name() ) ) {
            System.out.println( "EngineIO doesn't transpile" );
        }
        return endpoint;
    }

    private void tryReconnect() {
        if( this.reconnectTimeout != null ) {
            return;
        }

        int maxReconnectAttempts = Integer.parseInt( (String) options.get( "maxReconnectAttempts" ) );
        int reconnectIntervalIncrement = Integer.parseInt( (String) options.get( "reconnectIntervalIncrement" ) );

        if( this.reconnectionAttempt < maxReconnectAttempts ) {
            this.setState( ConnectionState.RECONNECTING );
            this.reconnectTimeout = new Timer();
            this.reconnectTimeout.schedule(new TimerTask() {
                public void run() {
                    tryOpen();
                }
            }, reconnectIntervalIncrement * this.reconnectionAttempt );
            this.reconnectionAttempt++;

        } else {
            this.clearReconnect();
            this.close();
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
