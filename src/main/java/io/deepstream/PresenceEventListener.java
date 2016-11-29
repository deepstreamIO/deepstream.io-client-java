package io.deepstream;


/**
 * An PresenceEventListener is notified when subscribed to client presence events. You can add an PresenceListener
 * via {@link PresenceHandler#subscribe(PresenceEventListener)}
 */
public interface PresenceEventListener {
    /**
     * Called with a the username of a client who logged in
     *
     * @param username the username of the client who logged in
     */
    void onClientLogin( String username );

    /**
     * Called with a the username of a client who logged out
     *
     * @param username the username of the client who logged out
     */
    void onClientLogout( String username );
}
