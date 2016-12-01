package io.deepstream;


/**
 * An PresenceEventListener is notified when subscribed to client presence events. You can add
 * a PresenceEventListener via {@link PresenceHandler#subscribe(PresenceEventListener)}
 */
public interface PresenceEventListener {
    /**
     * Called with the username of a client who logged in
     *
     * @param username the username of the client who logged in
     */
    void onClientLogin( String username );

    /**
     * Called with the username of a client who logged out
     *
     * @param username the username of the client who logged out
     */
    void onClientLogout( String username );
}
