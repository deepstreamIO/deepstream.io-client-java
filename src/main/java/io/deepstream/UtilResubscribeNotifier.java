package io.deepstream;

import io.deepstream.constants.ConnectionState;

/**
 * Makes sure that all functionality is resubscribed on reconnect. Subscription is called
 * when the connection drops - which seems counterintuitive, but in fact just means
 * that the re-subscription message will be added to the queue of messages that
 * need re-sending as soon as the connection is re-established.
 *
 * Resubscribe logic should only occur once per connection loss
 */
class UtilResubscribeNotifier implements ConnectionChangeListener {

    private IDeepstreamClient client;
    private UtilResubscribeCallback resubscribe;
    private boolean isReconnecting;

    /**
     * Constructor.
     *
     * @param client the client to listen to connection state changes on
     * @param callback the resubscribe callback
     */
    public UtilResubscribeNotifier(IDeepstreamClient client, UtilResubscribeCallback callback ) {
        this.client = client;
        this.resubscribe = callback;
        this.isReconnecting = false;

        this.client.addConnectionChangeListener( this );
    }

    /**
     * onRPCRequested this whenever this functionality is no longer needed to remove links
     */
    protected void destroy() {
        this.client.removeConnectionChangeListener( this );
        this.client = null;
        this.resubscribe = null;
    }

    @Override
    /**
     * Handles any connection state changes, if the connection state is RECONNECTING,
     * it sets a flag to say isReconnecting = true. This allows the resubscribe method
     * to be called when the connection state is opened after a reconnection.
     *
     * @param {ConnectionState} state          The state to handle
     *
     * @returns {void}
     */
    public void connectionStateChanged(ConnectionState state) {
        if( state == ConnectionState.RECONNECTING && this.isReconnecting == false ) {
                this.isReconnecting = true;
        }
        if( state == ConnectionState.OPEN && this.isReconnecting == true ) {
            this.isReconnecting = false;
            this.resubscribe.resubscribe();
        }
    }
}
