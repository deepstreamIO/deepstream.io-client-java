package io.deepstream;


import java.util.*;

public class PresenceHandler {

    private final int subscriptionTimeout;
    private final UtilEmitter emitter;
    private final DeepstreamConfig deepstreamConfig;
    private final IConnection connection;
    private final DeepstreamClientAbstract client;
    private final UtilAckTimeoutRegistry ackTimeoutRegistry;

    PresenceHandler(DeepstreamConfig deepstreamConfig, final IConnection connection, DeepstreamClientAbstract client) {
        this.subscriptionTimeout = deepstreamConfig.getSubscriptionTimeout();
        this.connection = connection;
        this.client = client;
        this.emitter = new UtilEmitter();
        this.deepstreamConfig = deepstreamConfig;
        this.ackTimeoutRegistry = client.getAckTimeoutRegistry();

        new UtilResubscribeNotifier(this.client, new UtilResubscribeNotifier.UtilResubscribeListener() {
            @Override
            public void resubscribe() {
                if( emitter.listeners(Topic.PRESENCE.name()).size() != 0 ) {
                    connection.sendMsg(Topic.PRESENCE, Actions.SUBSCRIBE, new String[]{Actions.SUBSCRIBE.name()});
                }
            }
        });
    }

    /**
     * Queries for clients logged into deepstream
     *
     * @param  presenceListener The presenceListener that will be called with an arraylist
     *                          of connected clients
     */
    public void getAll( PresenceListener presenceListener ) {
        if (!this.emitter.hasListeners( Actions.QUERY.toString() )) {
            this.connection.send( MessageBuilder.getMsg( Topic.PRESENCE, Actions.QUERY, Actions.QUERY.toString() ) );
        }
        this.emitter.on(Actions.QUERY.toString(), presenceListener);
    }

    /**
     * Removes the listener added via {@link EventHandler}
     *
     * @param eventListener The listener that will be called with the username of the client
     *                      and a boolean to indicated whether they logged in or out
     */
    public void subscribe( PresenceEventListener eventListener ) {
        if (!this.emitter.hasListeners(Topic.PRESENCE.toString())) {
            this.ackTimeoutRegistry.add( Topic.PRESENCE, Actions.SUBSCRIBE, Topic.PRESENCE.name(), this.subscriptionTimeout );
            this.connection.send(MessageBuilder.getMsg(Topic.PRESENCE, Actions.SUBSCRIBE, Actions.SUBSCRIBE.name()));
        }
        this.emitter.on(Topic.PRESENCE, eventListener);
    }

    /**
     * Removes the listener added via {@link EventHandler}
     *
     * @param eventListener The listener that will be called with the username of the client
     *                      and a boolean to indicated whether they logged in or out
     */
    public void unsubscribe( PresenceEventListener eventListener ) {
        this.emitter.off(Topic.PRESENCE.toString(), eventListener);
        if (this.emitter.hasListeners(Topic.PRESENCE.toString())) {
            this.ackTimeoutRegistry.add( Topic.PRESENCE,  Actions.UNSUBSCRIBE, Topic.PRESENCE.name(), this.subscriptionTimeout );
            this.connection.send(MessageBuilder.getMsg(Topic.PRESENCE, Actions.UNSUBSCRIBE, Actions.SUBSCRIBE.name()));
        }
    }


    protected void handle( Message message ) {
        if( message.action == Actions.ERROR && message.data[0].equals(Event.MESSAGE_DENIED.name()) ) {
            this.ackTimeoutRegistry.clear( message );
            this.client.onError( Topic.PRESENCE, Event.MESSAGE_DENIED, message.data[1] );
        }
        else if( message.action == Actions.ACK ) {
            this.ackTimeoutRegistry.clear( message );
        }
        else if( message.action == Actions.PRESENCE_JOIN ) {
            this.broadcastEvent( Topic.PRESENCE.name(), message.data[0], true );
        }
        else if( message.action == Actions.PRESENCE_LEAVE ) {
            this.broadcastEvent( Topic.PRESENCE.name(), message.data[0], false );
        }
        else if( message.action == Actions.QUERY ) {
            this.broadcastEvent( Actions.QUERY.name(), message.data );
        }
        else {
            this.client.onError( Topic.PRESENCE, Event.UNSOLICITED_MESSAGE, message.action.name() );
        }
    }

    private void broadcastEvent( String eventName, Object... args ) {
        java.util.List<Object> listeners = this.emitter.listeners( eventName );
        for( Object listener : listeners ) {
            if( args != null ) {
                ((EventListener) listener).onEvent(eventName, args);
            } else {
                ((EventListener) listener).onEvent(eventName);
            }
        }
    }
}
