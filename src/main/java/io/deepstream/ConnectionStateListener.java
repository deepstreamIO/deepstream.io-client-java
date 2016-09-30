package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

/**
 * A listener that will be notified whenever the ConnectionState changes. Can be added via
 * {@link DeepstreamClient#addConnectionChangeListener(ConnectionStateListener)} and removed via
 * {@link DeepstreamClient#removeConnectionChangeListener(ConnectionStateListener)}.
 */
public interface ConnectionStateListener {
    /**
     * Called with the new updated connection state. Useful for allowing your application to respond to different
     * scenarios, like {@link ConnectionState#ERROR} if an error occurs, or {@link ConnectionState#RECONNECTING} if
     * the connection drops.
     * @param connectionState The current connection state
     */
    @ObjectiveCName("connectionStateChanged:")
    void connectionStateChanged(ConnectionState connectionState );
}
