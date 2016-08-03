package io.deepstream;

import io.deepstream.constants.Actions;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.util.*;
import java.util.List;

public class EventHandler {

    private final int subscriptionTimeout;
    private final UtilEmitter emitter;
    private final Map options;
    private final IConnection connection;
    private final DeepstreamClientAbstract client;
    private final UtilAckTimeoutRegistry ackTimeoutRegistry;
    private final UtilResubscribeNotifier resubscribeNotifier;
    private final Map<String, UtilListener> listeners;
    private final List<String> subscriptions;

    public EventHandler(Map options, final IConnection connection, DeepstreamClientAbstract client ) {
        this.subscriptionTimeout = Integer.parseInt( (String) options.get( "subscriptionTimeout" ) );
        this.emitter = new UtilEmitter();
        this.connection = connection;
        this.client = client;
        this.options = options;
        this.listeners = new HashMap<>();
        this.subscriptions = new ArrayList<>();
        this.ackTimeoutRegistry = client.getAckTimeoutRegistry();

        this.resubscribeNotifier = new UtilResubscribeNotifier(this.client, new UtilResubscribeCallback() {
            @Override
            public void resubscribe() {
                for ( String eventName : subscriptions ) {
                    connection.sendMsg( Topic.EVENT, Actions.SUBSCRIBE, new String[] { eventName } );
                }
            }
        });
    }

    public void subscribe( String eventName, EventListener eventListener ) {
        if( this.emitter.hasListeners( eventName ) == false ) {
            this.subscriptions.add( eventName );
            this.ackTimeoutRegistry.add( Topic.EVENT, Actions.SUBSCRIBE, eventName, this.subscriptionTimeout );
            this.connection.send( MessageBuilder.getMsg( Topic.EVENT, Actions.SUBSCRIBE, eventName ) );
        }
        this.emitter.on( eventName, eventListener );
    }

    public void unsubscribe( String eventName, EventListener eventListener ) {
        this.subscriptions.remove( eventName );
        this.emitter.off(eventName, eventListener);
        if ( this.emitter.hasListeners(eventName) == false ) {
            this.ackTimeoutRegistry.add( Topic.EVENT,  Actions.UNSUBSCRIBE, eventName, this.subscriptionTimeout );
            this.connection.send(MessageBuilder.getMsg(Topic.EVENT, Actions.UNSUBSCRIBE, eventName));
        }
    }

    public void emit( String eventName ) {
        this.connection.send( MessageBuilder.getMsg( Topic.EVENT, Actions.EVENT, eventName ) );
        this.broadcastEvent( eventName );
    }

    public void emit( String eventName, Object data ) {
        this.connection.send( MessageBuilder.getMsg( Topic.EVENT, Actions.EVENT, eventName, MessageBuilder.typed( data ) ) );
        this.broadcastEvent( eventName, data );
    }

    public void listen( String pattern, ListenListener callback ) {
        if( this.listeners.get( pattern ) != null ) {
            this.client.onError( Topic.EVENT, Event.LISTENER_EXISTS, pattern );
        } else {
            this.listeners.put( pattern,
                    new UtilListener(Topic.EVENT, pattern, callback, this.options, this.client, this.connection )
            );
        }
    }

    public void unlisten( String pattern ) {
        UtilListener listener = this.listeners.get( pattern );
        if( listener != null ) {
            this.ackTimeoutRegistry.add( Topic.EVENT,  Actions.UNLISTEN, pattern, this.subscriptionTimeout );
            listener.destroy();
            this.listeners.remove( pattern );
        } else {
            this.client.onError( Topic.EVENT, Event.NOT_LISTENING, pattern );
        }
    }

    protected void handle( Message message ) {
        String eventName;

        if( message.action == Actions.ACK ) {
            eventName = message.data[ 1 ];
        } else {
            eventName = message.data[ 0 ];
        }

        if( message.action == Actions.EVENT ) {
            if( message.data.length == 2 ) {
                this.emit( eventName, MessageParser.convertTyped( message.data[ 1 ], this.client ) );
            } else {
                this.emit( eventName );
            }
        }
        else if( this.listeners.get( eventName ) != null ) {
            this.listeners.get( eventName ).onMessage( message );
        }
        else if( message.action == Actions.ACK ) {
            this.ackTimeoutRegistry.clear( message );
        }
        else if( message.action == Actions.ERROR ) {
            this.client.onError( Topic.EVENT, Event.getEvent( message.data[ 0 ] ), message.data[ 1 ]);
        }
        else {
            this.client.onError( Topic.EVENT, Event.UNSOLICITED_MESSAGE, eventName );
        }
    }

    private void broadcastEvent( String eventName, Object... args ) {
        List<Object> listeners = this.emitter.listeners( eventName );
        for( Object listener : listeners ) {
            if( args != null ) {
                ((EventListener) listener).onEvent(eventName, args);
            } else {
                ((EventListener) listener).onEvent(eventName);
            }
        }
    }
}
