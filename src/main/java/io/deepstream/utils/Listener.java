package io.deepstream.utils;


import io.deepstream.IConnection;
import io.deepstream.IDeepstreamClient;
import io.deepstream.constants.Actions;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;
import io.deepstream.message.Message;

import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

public class Listener implements ResubscribeCallback {

    private Topic type;
    private String pattern;
    private Emitter.Listener callback;
    private Map options;
    private IDeepstreamClient client;
    private IConnection connection;
    private ResubscribeNotifier resubscribeNotifier;

    private Timer timer;

    public Listener(Topic type, String pattern, Emitter.Listener callback, Map options, IDeepstreamClient client, IConnection connection ) {
        this.type = type;
        this.pattern = pattern;
        this.callback = callback;
        this.options = options;
        this.client = client;
        this.connection = connection;
        this.timer = new Timer();
        this.scheduleAckTimeout();
        this.resubscribeNotifier = new ResubscribeNotifier( this.client, this );
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

    private void onMessage( Message message ) {
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
        int subscriptionTimeout = (int) options.get( "subscriptionTimeout" );
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
