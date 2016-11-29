package io.deepstream;


/**
 * An PresenceEventListener is notified when subscribed to client presence events. You can add an PresenceListener
 * via {@link PresenceHandler#subscribe(PresenceEventListener)}
 */
public interface PresenceEventListener {
    /**
     * Called with a presence event (login/logout) of a client
     *
     * @param username the username of the client who logged in or out
     * @param event a boolean representing whether the user logged in or out
     */
    void onEvent( String username, boolean event );
}
