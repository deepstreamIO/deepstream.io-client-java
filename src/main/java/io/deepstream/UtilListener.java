package io.deepstream;

import io.deepstream.constants.Actions;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

class UtilListener implements UtilResubscribeCallback {

    private Topic type;
    private String pattern;
    private UtilEmitter.Listener callback;
    private Map options;
    private IDeepstreamClient client;
    private IConnection connection;
    private UtilResubscribeNotifier resubscribeNotifier;

    private Timer timer;

    public UtilListener(Topic type, String pattern, UtilEmitter.Listener callback, Map options, IDeepstreamClient client, IConnection connection ) {
        this.type = type;
        this.pattern = pattern;
        this.callback = callback;
        this.options = options;
        this.client = client;
        this.connection = connection;
        this.timer = new Timer();
        this.scheduleAckTimeout();
        this.resubscribeNotifier = new UtilResubscribeNotifier( this.client, this );
        this.sendListen();
    }

    public void destroy() {
        this.connection.sendMsg( this.type, Actions.UNLISTEN, new String[] { this.pattern } );
        this.resubscribeNotifier.destroy();
        this.callback = null;
        this.pattern = null;
        this.client = null;
        this.connection = null;
        this.timer = null;
    }

    public void onMessage( Message message ) {
        if( message.action.equals( Actions.ACK ) ) {
            this.timer.cancel();
        } else {
            boolean isFound = message.action.equals( Actions.SUBSCRIBTION_FOR_PATTERN_FOUND );
            this.callback.call( message.data[ 1 ], isFound );
        }
    }

    private void sendListen() {
        this.connection.sendMsg( this.type, Actions.LISTEN, new String[] { this.pattern } );
    }

    private void scheduleAckTimeout() {
        int subscriptionTimeout = Integer.parseInt( (String) options.get( "subscriptionTimeout" ) );
        this.timer.schedule(new TimerTask() {
            public void run() {
                client.onError( type, Event.ACK_TIMEOUT, "No ACK message received in time for " + pattern );
            }
        }, subscriptionTimeout);
    }

    @Override
    public void resubscribe() {
        sendListen();
    }
}
