package io.deepstream.utils;

public interface AckTimeoutCallback {
    void onTimeout( String name );
}
