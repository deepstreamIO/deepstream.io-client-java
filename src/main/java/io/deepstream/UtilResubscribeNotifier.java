package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

/**
 * Makes sure that all functionality is resubscribed on reconnect. Subscription is called
 * when the connection drops - which seems counterintuitive, but in fact just means
 * that the re-subscription message will be added to the queue of messages that
 * need re-sending as soon as the connection is re-established.
 *
 * Resubscribe logic should only occur once per connection loss
 */
class UtilResubscribeNotifier implements ConnectionStateListener {

    private DeepstreamClientAbstract client;
    private UtilResubscribeListener resubscribe;
    private boolean isReconnecting;

    /**
     * Constructor.
     *
     * @param client the client to listen to connection state changes on
     * @param callback the resubscribe callback
     */
    @ObjectiveCName("init:callback:")
    public UtilResubscribeNotifier(DeepstreamClientAbstract client, UtilResubscribeListener callback) {
        this.client = client;
        this.resubscribe = callback;
        this.isReconnecting = false;

        this.client.addConnectionChangeListener( this );
    }

    /**
     * onRPCRequested this whenever this functionality is no longer needed to remove links
     */
    void destroy() {
        this.client.removeConnectionChangeListener( this );
        this.client = null;
        this.resubscribe = null;
    }

    /**
     * @see ConnectionStateListener
     */
    @Override
    @ObjectiveCName("connectionStateChanged:")
    public void connectionStateChanged(ConnectionState state) {
        if( state == ConnectionState.RECONNECTING && !this.isReconnecting) {
                this.isReconnecting = true;
        }
        if( state == ConnectionState.OPEN && this.isReconnecting) {
            this.isReconnecting = false;
            this.resubscribe.resubscribe();
        }
    }

    interface UtilResubscribeListener {
        void resubscribe();
    }
}
