package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

class UtilSingleNotifier implements UtilResubscribeNotifier.UtilResubscribeListener, UtilTimeoutListener {

    private final Topic topic;
    private final Actions action;
    private final int timeoutDuration;
    private final IConnection connection;
    private final Map<String, ArrayList<UtilSingleNotifierCallback> > requests;
    private final UtilAckTimeoutRegistry ackTimeoutRegistry;

    /**
     * Provides a scaffold for subscriptionless requests to io.deepstream.gherkin, such as the SNAPSHOT
     * and HAS functionality. The SingleNotifier multiplexes all the client requests so
     * that they can can be notified at once, and also includes reconnection funcionality
     * incase the connection drops.
     * @param client The deepstream client
     * @param connection The deepstream connection
     * @param topic The Topic
     * @param action The Action
     * @param timeoutDuration The timeout duration before an ack timeout is triggered
     */
    @ObjectiveCName("init:connection:topic:action:timeoutDuration:")
    public UtilSingleNotifier(DeepstreamClientAbstract client, IConnection connection, Topic topic, Actions action, int timeoutDuration ) {
        this.ackTimeoutRegistry = client.getAckTimeoutRegistry();
        this.connection = connection;
        this.topic = topic;
        this.action = action;
        this.timeoutDuration = timeoutDuration;

        new UtilResubscribeNotifier(client, this);
        requests = new ConcurrentHashMap<>();
    }

    /**
     * Check if there is a request pending with a specified name
     * @param name The name of the object being requested
     * @return true if the request exists
     */
    @ObjectiveCName("hasRequest:")
    public boolean hasRequest( String name ) {
        return requests.containsKey( name );
    }

    /**
     * Add a request. If one has already been made it will skip the server request
     * and multiplex the response
     *
     * @param name The name of the object being requested
     * @param utilSingleNotifierCallback The callback to call once the request is completed
     */
    @ObjectiveCName("request:utilSingleNotifierCallback:")
    public void request( String name, UtilSingleNotifierCallback utilSingleNotifierCallback ) {
        ArrayList<UtilSingleNotifierCallback> callbacks = requests.get( name );
        if( callbacks == null ) {
            synchronized (this) {
                callbacks = new ArrayList<>();
                requests.put(name, callbacks);
                send(name);
            }
        }

        callbacks.add( utilSingleNotifierCallback );
        ackTimeoutRegistry.add(topic, action, name, Event.RESPONSE_TIMEOUT, this, timeoutDuration);
    }

    /**
     * Process a response for a request. This has quite a flexible API since callback functions
     * differ greatly and helps maximise reuse.
     *
     * @param name The name of the object being requested
     * @param error An error that may have occurred during the request
     * @param data The result data from the request
     */
    @ObjectiveCName("recieve:error:data:")
    public void recieve(String name, DeepstreamError error, Object data) {
        ArrayList<UtilSingleNotifierCallback> callbacks = requests.get( name );
        for (UtilSingleNotifierCallback callback : callbacks) {
            ackTimeoutRegistry.clear(topic, action, name);
            if( error != null ) {
                callback.onSingleNotifierError( name, error );
            } else {
                callback.onSingleNotifierResponse( name, data );
            }
        }
        requests.remove( name );
    }

    @Override
    public void resubscribe() {
        for( Object name : requests.keySet() ) {
            send( (String) name );
        }
    }

    @ObjectiveCName("send:")
    private void send( String name ) {
        connection.send( MessageBuilder.getMsg( topic, action, name ) );
    }

    @Override
    @ObjectiveCName("onTimeout:action:event:name:")
    public void onTimeout(Topic topic, Actions action, Event event, String name) {
        this.recieve(name, new DeepstreamError(String.format("Response for % timed out", name)), null);
    }

    interface UtilSingleNotifierCallback {
        @ObjectiveCName("onSingleNotifierError:error:")
        void onSingleNotifierError(String name, DeepstreamError error);
        
        @ObjectiveCName("onSingleNotifierResponse:data:")
        void onSingleNotifierResponse(String name, Object data);
    }
}
