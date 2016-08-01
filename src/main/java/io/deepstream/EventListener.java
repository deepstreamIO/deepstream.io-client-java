package io.deepstream;

/**
 * An EventListener is notified whenever it is triggered locally or remotely. You can add an EventListener via {@link EventHandler#subscribe(String, EventListener)}
 */
public interface EventListener {
    /**
     * Triggered whenever an event you have subscribed to is triggered,
     * with a flexible array of arguments
     * @param eventName The event name that has occurred
     * @param args The arguments that the event has been triggered with
     */
    void onEvent( String eventName, Object... args );
}
