package io.deepstream;

import java.util.*;
import java.util.List;

/**
 * The entry point for events, such as {@link EventHandler#subscribe(String, EventListener)},
 * {@link EventHandler#emit(String)} and provider functionality such as {@link EventHandler#listen(String, ListenListener)}
 */
public class EventHandler {

    private final int subscriptionTimeout;
    private final UtilEmitter emitter;
    private final DeepstreamConfig deepstreamConfig;
    private final IConnection connection;
    private final DeepstreamClientAbstract client;
    private final UtilAckTimeoutRegistry ackTimeoutRegistry;
    private final Map<String, UtilListener> listeners;
    private final List<String> subscriptions;

    EventHandler(DeepstreamConfig deepstreamConfig, final IConnection connection, DeepstreamClientAbstract client) {
        this.subscriptionTimeout = deepstreamConfig.getSubscriptionTimeout();
        this.emitter = new UtilEmitter();
        this.connection = connection;
        this.client = client;
        this.deepstreamConfig = deepstreamConfig;
        this.listeners = new HashMap<>();
        this.subscriptions = new ArrayList<>();
        this.ackTimeoutRegistry = client.getAckTimeoutRegistry();

        new UtilResubscribeNotifier(this.client, new UtilResubscribeNotifier.UtilResubscribeListener() {
            @Override
            public void resubscribe() {
                for (String eventName : subscriptions) {
                    connection.sendMsg(Topic.EVENT, Actions.SUBSCRIBE, new String[]{eventName});
                }
            }
        });
    }

    /**
     * Subscribes to eventName and notifies the listener via {@link EventListener}
     * whenever it occurs locally or remotely
     *
     * @param eventName     The event name
     * @param eventListener The eventListener
     */
    public void subscribe( String eventName, EventListener eventListener ) {
        if (this.emitter.hasListeners(eventName)) {
            this.subscriptions.add( eventName );
            this.ackTimeoutRegistry.add( Topic.EVENT, Actions.SUBSCRIBE, eventName, this.subscriptionTimeout );
            this.connection.send( MessageBuilder.getMsg( Topic.EVENT, Actions.SUBSCRIBE, eventName ) );
        }
        this.emitter.on(eventName, eventListener);
    }

    /**
     * Removes the listener added via {@link EventHandler}
     *
     * @param eventName     The event name
     * @param eventListener The listener that was previous added
     */
    public void unsubscribe( String eventName, EventListener eventListener ) {
        this.subscriptions.remove( eventName );
        this.emitter.off(eventName, eventListener);
        if (this.emitter.hasListeners(eventName)) {
            this.ackTimeoutRegistry.add( Topic.EVENT,  Actions.UNSUBSCRIBE, eventName, this.subscriptionTimeout );
            this.connection.send(MessageBuilder.getMsg(Topic.EVENT, Actions.UNSUBSCRIBE, eventName));
        }
    }

    /**
     * @see EventHandler
     */
    public void emit( String eventName ) {
        this.connection.send( MessageBuilder.getMsg( Topic.EVENT, Actions.EVENT, eventName));
        this.broadcastEvent(eventName);
    }

    /**
     * Emit an event that you want all subscribers, both local and remote to be informed of. You
     * can provide any object that can be serialised to json
     * @param eventName the event name
     * @param data the data to serialise and send with the event
     */
    public void emit( String eventName, Object data ) {
        this.connection.send( MessageBuilder.getMsg( Topic.EVENT, Actions.EVENT, eventName, MessageBuilder.typed( data)));
        this.broadcastEvent(eventName, data);
    }

    /**
     * Listen to any subscriptions that have been made on the system that match the provided
     * pattern. If that subscription is found it will give the client the opportunity to accept
     * the role of being the provider, or rejecting it to pass responsibility to someone else.
     *
     * @param pattern        The pattern to match, must be valid regex
     * @param listenListener The listener to inform the client when a subscription
     *                       has been found or removed
     */
    public void listen(String pattern, ListenListener listenListener ) {
        if( this.listeners.get( pattern ) != null ) {
            this.client.onError( Topic.EVENT, Event.LISTENER_EXISTS, pattern );
        } else {
            synchronized (this) {
                UtilListener eventListener = new UtilListener(Topic.EVENT, pattern, listenListener, this.deepstreamConfig, this.client, this.connection);
                this.listeners.put(pattern, eventListener);
                eventListener.start();
            }
        }
    }

    /**
     * Remove the listener added via {@link EventHandler}, this will remove
     * the provider as the active provider and allow another provider to take its place
     * @param pattern The pattern that has been previously listened to
     */
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
                this.broadcastEvent( eventName, MessageParser.convertTyped(message.data[1], this.client) );
            } else {
                this.broadcastEvent(eventName);
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
