package io.deepstream.message;

import io.deepstream.ConnectionChangeListener;
import io.deepstream.DeepstreamClient;
import io.deepstream.LoginCallback;
import io.deepstream.constants.Actions;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Topic;
import io.socket.emitter.Emitter;
import io.socket.engineio.client.Socket;
import org.json.JSONObject;

import java.net.URISyntaxException;
import java.util.*;

public class Connection {

    private DeepstreamClient client;
    private String originalUrl;
    private Socket socket;
    private ConnectionState connectionState;
    private ArrayList<ConnectionChangeListener> connectStateListeners;

    private StringBuilder messageBuffer;

    private LoginCallback loginCallback;
    private JSONObject authParameters;

    public Connection(final String url, final Map options, DeepstreamClient client ) throws URISyntaxException {
        System.out.println( "Connecting to " + url );
        this.client = client;
        this.connectStateListeners = new ArrayList<ConnectionChangeListener>();
        this.originalUrl = url;
        this.connectionState = ConnectionState.CLOSED;
        this.messageBuffer = new StringBuilder();

        this.socket = new Socket( url );
        this.addConnectionListeners();
        this.socket.open();
    }

    public void authenticate( JSONObject authParameters, LoginCallback loginCallback ) {
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
            this.socket.send( message );
        }
    }

    private void sendAuthMessage() {
        String authMessage = MessageBuilder.getMsg( Topic.AUTH, Actions.REQUEST, this.authParameters.toString() );
        this.socket.send( authMessage );
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

    private void onOpen() {
        this.setState( ConnectionState.AWAITING_CONNECTION );
    }

    private void onError( Exception exception ) {
        System.out.println( exception );
    }

    private void onMessage( String rawMessage ) {
        System.out.println( "Message received: " + rawMessage );
        List<Message> parsedMessages = MessageParser.parse( rawMessage, this );
        for( short i=0; i<parsedMessages.size(); i++ ) {
            Message message = parsedMessages.get( i );
            if( message.topic == Topic.CONNECTION ) {
                handleConnectionResponse( message  );
            }
            else if( message.topic == Topic.AUTH ) {
                handleAuthResponse( message );
            }
            else if( message.topic == Topic.EVENT ){
                this.client.event.handle( message );
            }
            else {
                System.out.println( "Normal message of type " + message.topic );
            }
        }
    }

    private void handleConnectionResponse( Message message ) {
        if( message.action == Actions.ACK ) {
            this.setState( ConnectionState.AWAITING_AUTHENTICATION );
        }
        else if( message.action == Actions.CHALLENGE ) {
            this.setState( ConnectionState.CHALLENGING );
            this.socket.send( MessageBuilder.getMsg( Topic.CONNECTION, Actions.CHALLENGE_RESPONSE,  this.originalUrl ) );
        }
    }

    private void handleAuthResponse( Message message ) {
        if( message.action == Actions.ERROR ) {
            if( this.loginCallback != null ) {
                this.loginCallback.loginFailed( message.data[ 1 ] );
            }
        }
        else if( message.action == Actions.ACK ) {
            this.setState( connectionState.OPEN );

            if( this.messageBuffer.length() > 0 ) {
                System.out.println( "Flushing initial buffer: " + this.messageBuffer.toString() );
                this.socket.send( this.messageBuffer.toString() );
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

    private void addConnectionListeners() {
        this.socket.on( Socket.EVENT_OPEN, new Emitter.Listener() {
            public void call(Object... args) {
                Connection.this.onOpen();
            }
        } );

        this.socket.on( Socket.EVENT_MESSAGE, new Emitter.Listener() {
            public void call(Object... args) {
                Connection.this.onMessage( (String)args[0] );
            }
        });

        this.socket.on(Socket.EVENT_ERROR, new Emitter.Listener() {
            public void call(Object... args) {
                Connection.this.onError( (Exception)args[ 0 ] );
            }
        });
    }
}
