package io.deepstream.message;

import io.deepstream.ConnectionChangeListener;
import io.deepstream.LoginCallback;
import io.deepstream.constants.Actions;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Topic;
import io.socket.emitter.Emitter;
import io.socket.engineio.client.Socket;
import jdk.nashorn.api.scripting.JSObject;
import org.json.JSONObject;

import java.net.URISyntaxException;
import java.util.*;

public class Connection {

    private String originalUrl;
    private Socket socket;
    private ConnectionState connectionState;
    private ArrayList<ConnectionChangeListener> connectStateListeners;

    private LoginCallback loginCallback;
    private JSONObject authParameters;

    public Connection( final String url, final Map options ) throws URISyntaxException {
        System.out.println( "Connecting to " + url );
        this.connectStateListeners = new ArrayList<ConnectionChangeListener>();
        this.originalUrl = url;
        this.connectionState = ConnectionState.CLOSED;

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

    private void sendAuthMessage() {
        String authMessage = MessageBuilder.getMsg( Topic.AUTH, Actions.REQUEST, this.authParameters.toString() );
        System.out.println( authMessage );
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

    public void send( Message message ) {
        this.socket.send( MessageBuilder.getMsg( message ) );
    }

    private void onOpen() {
        this.setState( ConnectionState.AWAITING_CONNECTION );
    }

    private void onError( Exception exception ) {
        System.out.println( exception );
    }

    private void onMessage( String rawMessage ) {
        System.out.println( rawMessage );
        List<Message> parsedMessages = MessageParser.parse( rawMessage, this );
        for( short i=0; i<parsedMessages.size(); i++ ) {
            Message message = parsedMessages.get( i );
            if( message.topic == Topic.CONNECTION ) {
                handleConnectionResponse( message  );
            }
            else if( message.topic == Topic.AUTH ) {
                handleAuthResponse( message );
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
