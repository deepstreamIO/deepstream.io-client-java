package io.deepstream;

import io.deepstream.constants.ConnectionState;

/**
 * A listener that will be notified whenever the ConnectionState changes. Can be added via
 * {@link DeepstreamClient#addConnectionChangeListener(ConnectionChangeListener)} and removed via
 * {@link DeepstreamClient#removeConnectionChangeListener(ConnectionChangeListener)}.
 */
public interface ConnectionChangeListener {
    /**
     * Called with the new updated connection state. Useful for allowing your application to respond to different
     * scenarios, like {@link ConnectionState#ERROR} if an error occurs, or {@link ConnectionState#RECONNECTING} if
     * the connection drops.
     * @param connectionState
     */
    void connectionStateChanged(ConnectionState connectionState );
}
