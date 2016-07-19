package io.deepstream;

import io.deepstream.constants.Actions;
import io.deepstream.constants.Topic;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class UtilSingleNotifier {

    private final Topic topic;
    private final Actions action;
    private final int timeoutDuration;
    private final IDeepstreamClient client;
    private final Connection connection;
    private final Map requests;

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
    public UtilSingleNotifier(IDeepstreamClient client, Connection connection, Topic topic, Actions action, int timeoutDuration ) {
        this.ackTimeoutRegistry = client.
        this.client = client;
        this.connection = connection;
        this.topic = topic;
        this.action = action;
        this.timeoutDuration = timeoutDuration;

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

    public void recieve( String name, Object error, Object data ) {
        ArrayList<UtilSingleNotifierCallback> callbacks = (ArrayList<UtilSingleNotifierCallback>) requests.get( name );
        for (UtilSingleNotifierCallback callback : callbacks) {
            if( error != null ) {
                callback.onSingleNotifierError( name, error );
            } else {
                callback.onSingleNotifierResponse( name, data );
            }
        }
    }
}
