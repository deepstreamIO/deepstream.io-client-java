package io.deepstream;


/**
 * An PresenceListener is notified querying for clients. You can add an PresenceListener
 * via {@link PresenceHandler#getAll(PresenceListener)}
 */
public interface PresenceListener {
    /**
     * Called when querying for clients
     *
     * @param args An array of client usernames who are currently logged in
     */
    void onClients( Object... args );
}
