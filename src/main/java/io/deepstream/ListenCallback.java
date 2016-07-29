package io.deepstream;

public interface ListenCallback {
    void onSubscriptionForPatternAdded( String subscription );
    void onSubscriptionForPatternRemoved( String subscription );
}
