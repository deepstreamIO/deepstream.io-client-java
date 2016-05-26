package io.deepstream.message;

import com.google.gson.JsonObject;
import io.deepstream.*;
import io.deepstream.constants.*;

import java.net.URISyntaxException;
import java.util.*;

public class Connection {

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
            this.setState( ConnectionState.AUTHENTICATING );
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

    private void sendAuthMessage() {
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

        try {
            this.endpoint.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    void onOpen() {
        this.setState( ConnectionState.AWAITING_CONNECTION );
    }

    void onError( String error ) {
        //System.out.println( error );
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
        else if( this.originalUrl.equals( this.url ) == false ) {
            this.url = this.originalUrl;
            this.createEndpoint();
        }
    }

    private void handleConnectionResponse( Message message ) {
        if( message.action == Actions.ACK ) {
            this.setState( ConnectionState.AWAITING_AUTHENTICATION );
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

        System.out.println( options.get( "endpoint") );
        if( options.get( "endpoint" ).equals( EndpointType.TCP.name() ) ) {
           endpoint = new EndpointTCP( url, options, this );
        } else if( options.get( "endpoint" ).equals( EndpointType.ENGINEIO.name() ) ) {
            System.out.println( "EngineIO doesn't transpile" );
        }
        return endpoint;
    }
}
