package io.deepstream;

public interface EventCallback {
    void onEvent( String eventName, Object... args );
}
