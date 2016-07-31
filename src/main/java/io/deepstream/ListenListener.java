package io.deepstream;

/**
 * An interface that notifies whenever a pattern match has been found
 * TODO: Entirely changes with new listener functionality
 */
public interface ListenListener {
    void onSubscriptionForPatternAdded( String subscription );
    void onSubscriptionForPatternRemoved( String subscription );
}
