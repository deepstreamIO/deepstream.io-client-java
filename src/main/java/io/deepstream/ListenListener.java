package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

/**
 * An interface that notifies whenever a pattern match has been found or removed when added via {@link RecordHandler#listen(String, ListenListener)}
 * or {@link EventHandler#listen(String, ListenListener)}
 */
public interface ListenListener {
    /**
     * Called whenever a subscription has been found a pattern you previously added.
     * </br>
     * This method must return a true if it is willing to provide the subscription, and false otherwise. This also has
     * to be done in a timely fashion since the server will assume the provider is unresponsive if it takes too long.
     *
     * @param subscription The name of the subscription that can be provided
     * @return true if the server responds is willing to accept the request, false to reject
     */
    @ObjectiveCName("onSubscriptionForPatternAdded:")
    boolean onSubscriptionForPatternAdded( String subscription );

    /**
     * If a provider has accepted a request, they will then be notified when the subscription is no longer needed
     * so that they can stop providing
     *
     * @param subscription The name of the subscription to stop providing
     */
    @ObjectiveCName("onSubscriptionForPatternRemoved:")
    void onSubscriptionForPatternRemoved( String subscription );
}
