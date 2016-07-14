package io.deepstream;

import io.deepstream.constants.Actions;
import io.deepstream.constants.Topic;

import java.util.Map;

class UtilListener implements UtilResubscribeCallback {

    private UtilAckTimeoutRegistry ackTimoutRegistry;
    private Topic topic;
    private String pattern;
    private Emitter.Listener callback;
    private Map options;
    private IDeepstreamClient client;
    private IConnection connection;
    private UtilResubscribeNotifier resubscribeNotifier;

    public UtilListener(Topic topic, String pattern, Emitter.Listener callback, Map options, IDeepstreamClient client, IConnection connection ) {
        this.topic = topic;
        this.pattern = pattern;
        this.callback = callback;
        this.options = options;
        this.client = client;
        this.connection = connection;
        this.resubscribeNotifier = new UtilResubscribeNotifier( this.client, this );
        this.ackTimoutRegistry = UtilAckTimeoutRegistry.getAckTimeoutRegistry( this.client );
        this.scheduleAckTimeout();
        this.sendListen();
    }

    public void destroy() {
        this.connection.sendMsg( this.topic, Actions.UNLISTEN, new String[] { this.pattern } );
        this.resubscribeNotifier.destroy();
        this.callback = null;
        this.pattern = null;
        this.client = null;
        this.connection = null;
        this.ackTimoutRegistry = null;
    }

    public void onMessage( Message message ) {
        if( message.action.equals( Actions.ACK ) ) {
            this.ackTimoutRegistry.clear( message );
        } else {
            boolean isFound = message.action.equals( Actions.SUBSCRIBTION_FOR_PATTERN_FOUND );
            this.callback.call( message.data[ 1 ], isFound );
        }
    }

    private void sendListen() {
        this.connection.sendMsg( this.topic, Actions.LISTEN, new String[] { this.pattern } );
    }

    private void scheduleAckTimeout() {
        int subscriptionTimeout = Integer.parseInt( (String) options.get( "subscriptionTimeout" ) );
        this.ackTimoutRegistry.add( this.topic, Actions.LISTEN, this.pattern, subscriptionTimeout );
    }

    @Override
    public void resubscribe() {
        this.scheduleAckTimeout();
        sendListen();
    }
}
