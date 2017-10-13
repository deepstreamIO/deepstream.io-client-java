package io.deepstream;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.j2objc.annotations.ObjectiveCName;

import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;

class UtilSingleNotifier implements UtilResubscribeNotifier.UtilResubscribeListener, UtilTimeoutListener {

    private final Topic topic;
    private final Actions action;
    private final int timeoutDuration;
    private final IConnection connection;
    private final ConcurrentHashMap<String, ConcurrentLinkedQueue<UtilSingleNotifierCallback>> requests;
    private final UtilAckTimeoutRegistry ackTimeoutRegistry;
    private final UtilResubscribeNotifier utilResubscribeNotifier;

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

        this.utilResubscribeNotifier = new UtilResubscribeNotifier(client, this);
        this.requests = new ConcurrentHashMap<String, ConcurrentLinkedQueue<UtilSingleNotifierCallback>>();
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
        this.request(name, null, null, utilSingleNotifierCallback);
        ackTimeoutRegistry.add(topic, action, name, Event.RESPONSE_TIMEOUT, this, timeoutDuration);
    }

    /**
     * Add a request where a response may contain more than one bit of data. Commonly used with
     * {@link UtilSingleNotifier#recieve(JsonArray, DeepstreamError)}
     *
     * @param name The name or version to store callbacks on
     * @param data The data to send in the request
     * @param action The action to send with the request
     * @param utilSingleNotifierCallback The callback to call once the request is completed
     */
    public void request( String name, Actions action, String[] data, UtilSingleNotifierCallback utilSingleNotifierCallback ) {
        ConcurrentLinkedQueue<UtilSingleNotifierCallback> callbacks = requests.get(name);
        if (callbacks == null) {
            callbacks = new ConcurrentLinkedQueue<UtilSingleNotifierCallback>();
            ConcurrentLinkedQueue<UtilSingleNotifierCallback> prevCallbacks = requests.putIfAbsent(name, callbacks);
            if( prevCallbacks == null ) {
                if (action != null) {
                    send(action, data);
                } else {
                    send(name);
                }
            } else {
                callbacks = prevCallbacks;
            }
        }

        callbacks.add(utilSingleNotifierCallback);
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
        ConcurrentLinkedQueue<UtilSingleNotifierCallback> callbacks = requests.remove( name );
        for (UtilSingleNotifierCallback callback : callbacks) {
            ackTimeoutRegistry.clear(topic, action, name);
            if( error != null ) {
                callback.onSingleNotifierError( name, error );
            } else {
                callback.onSingleNotifierResponse( name, data );
            }
        }
    }

    /**
     * Process a response for a request. This overload of the method is for cases where
     * data from multiple messages has been merged into one deepstream message to save network
     * traffic.
     *
     * Used in conjunction with {@link UtilSingleNotifier#request(String, Actions, String[], UtilSingleNotifierCallback)}
     *
     * @param data The data received in the message
     * @param error Any errors from the message
     */
    public void recieve(JsonArray data, DeepstreamError error) {
        for (JsonElement version : data) {
            ConcurrentLinkedQueue<UtilSingleNotifierCallback> callbacks = requests.remove( version.getAsString() );
            UtilSingleNotifierCallback cb = callbacks.peek();
            if( error != null) {
                cb.onSingleNotifierError(null, error);
            } else {
                cb.onSingleNotifierResponse(null, null);
            }
        }
    }

    void destroy() {
        this.utilResubscribeNotifier.destroy();
        this.requests.clear();
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

    private void send( Actions action, String[] data ) {
        connection.send( MessageBuilder.getMsg( topic, action, data ) );
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
