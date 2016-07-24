package io.deepstream;

import io.deepstream.constants.Actions;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class UtilSingleNotifier implements UtilResubscribeCallback {

    private final Topic topic;
    private final Actions action;
    private final int timeoutDuration;
    private final IDeepstreamClient client;
    private final IConnection connection;
    private final Map requests;
    private final UtilAckTimeoutRegistry ackTimeoutRegistry;
    private final UtilResubscribeNotifier resubscribeNotifier;

    /**
     * Provides a scaffold for subscriptionless requests to deepstream, such as the SNAPSHOT
     * and HAS functionality. The SingleNotifier multiplexes all the client requests so
     * that they can can be notified at once, and also includes reconnection funcionality
     * incase the connection drops.
     * @param client
     * @param connection
     * @param topic
     * @param action
     * @param timeoutDuration
     */
    public UtilSingleNotifier(IDeepstreamClient client, IConnection connection, Topic topic, Actions action, int timeoutDuration ) {
        this.ackTimeoutRegistry = client.getAckTimeoutRegistry();
        this.client = client;
        this.connection = connection;
        this.topic = topic;
        this.action = action;
        this.timeoutDuration = timeoutDuration;

        resubscribeNotifier = new UtilResubscribeNotifier(client, this);
        requests = new HashMap<String, ArrayList>();
    }

    /**
     * Check if there is a request pending with a specified name
     * @param name
     * @return
     */
    public boolean hasRequest( String name ) {
        return requests.containsKey( name );
    }

    /**
     * Add a request. If one has already been made it will skip the server request
     * and multiplex the response
     *
     * @param name
     * @param callback
     */
    public void request( String name, UtilSingleNotifierCallback callback ) {
        ArrayList callbacks = (ArrayList) requests.get( name );
        if( callbacks == null ) {
            callbacks = new ArrayList();
            requests.put( name, callbacks );
            send( name );
        }

        callbacks.add( callback );
        ackTimeoutRegistry.add( topic, action, name, Event.RESPONSE_TIMEOUT, timeoutDuration );
    }

    /**
     * Process a response for a request. This has quite a flexible API since callback functions
     * differ greatly and helps maximise reuse.
     *
     * @param name
     * @param error
     * @param data
     */
    public void recieve( String name, DeepstreamException error, Object data ) {
        ArrayList<UtilSingleNotifierCallback> callbacks = (ArrayList<UtilSingleNotifierCallback>) requests.get( name );
        for (UtilSingleNotifierCallback callback : callbacks) {
            if( error != null ) {
                callback.onSingleNotifierError( name, error );
            } else {
                callback.onSingleNotifierResponse( name, data );
            }
            ackTimeoutRegistry.clear( topic, action, name );
        }
        requests.remove( name );
    }

    @Override
    public void resubscribe() {
        for( Object name : requests.keySet() ) {
            send( (String) name );
        }
    }

    private void send( String name ) {
        connection.send( MessageBuilder.getMsg( topic, action, name ) );
    }
}
