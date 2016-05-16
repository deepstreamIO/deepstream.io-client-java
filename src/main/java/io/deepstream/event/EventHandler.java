package io.deepstream.event;

import io.deepstream.DeepstreamClient;
import io.deepstream.constants.Actions;
import io.deepstream.constants.Topic;
import io.deepstream.message.Connection;
import io.deepstream.message.Message;
import io.deepstream.message.MessageBuilder;
import io.deepstream.message.MessageParser;
import io.deepstream.utils.ResubscribeCallback;
import io.deepstream.utils.ResubscribeNotifier;
import io.socket.emitter.Emitter;
import java.util.Map;

public class EventHandler implements ResubscribeCallback {

    private Emitter emitter;
    private Map options;
    private Connection connection;
    private DeepstreamClient client;
    private ResubscribeNotifier resubscribeNotifier;

    public EventHandler( Map options, Connection connection, DeepstreamClient client ) {
        this.emitter = new Emitter();
        this.connection = connection;
        this.client = client;
        this.resubscribeNotifier = new ResubscribeNotifier( this.client, this );
    }

    public void subscribe( String eventName, Emitter.Listener eventListener ) {
        if( this.emitter.hasListeners( eventName ) == false ) {
            this.connection.send( MessageBuilder.getMsg( Topic.EVENT, Actions.SUBSCRIBE, eventName ) );
        }
        this.emitter.on( eventName, eventListener );
    }

    public void unsubscribe( String eventName, Emitter.Listener eventListener ) {
        this.emitter.off(eventName, eventListener);
        if ( this.emitter.hasListeners(eventName) == false ) {
            this.connection.send(MessageBuilder.getMsg(Topic.EVENT, Actions.UNSUBSCRIBE, eventName));
        }
    }

    public void emit( String eventName ) {
        this.connection.send( MessageBuilder.getMsg( Topic.EVENT, Actions.EVENT, eventName ) );
        this.emitter.emit( eventName );
    }

    public void emit( String eventName, Object data ) {
        this.connection.send( MessageBuilder.getMsg( Topic.EVENT, Actions.EVENT, eventName, MessageBuilder.typed( data ) ) );
        this.emitter.emit( eventName, data );
    }

    public void handle( Message message ) {
        String eventName;

        if( message.action == Actions.ACK ) {
            eventName = message.data[ 1 ];
        } else {
            eventName = message.data[ 0 ];
        }

        if( message.action == Actions.EVENT ) {
            if( message.data[ 1 ] != null ) {
                this.emitter.emit( eventName, MessageParser.convertTyped( message.data[ 1 ] ) );
            } else {
                this.emitter.emit( eventName );
            }
        }
        else if( message.action == Actions.ACK ) {
            System.out.println( "Recieved ack for " + eventName );
        }
        else if( message.action == Actions.ERROR ) {
            System.out.println( "Recieved error for " + eventName );
        }else {
            System.out.println( "Unsoliciated message " + eventName );
        }
    }

    @Override
    public void resubscribe() {

    }
}
