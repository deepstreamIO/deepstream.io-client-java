package io.deepstream;

public interface EventCallback {
    /**
     * Triggered whenever an event you have subscribed to is triggered,
     * with a flexible array of arguments
     * @param eventName
     * @param args
     */
    void onEvent( String eventName, Object... args );
}
