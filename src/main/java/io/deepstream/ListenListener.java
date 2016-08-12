package io.deepstream;

/**
 * An interface that notifies whenever a pattern match has been found
 */
public interface ListenListener {
    boolean onSubscriptionForPatternAdded( String subscription );
    void onSubscriptionForPatternRemoved( String subscription );
}
