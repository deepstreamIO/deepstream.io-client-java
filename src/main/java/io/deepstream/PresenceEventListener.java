package io.deepstream;


public interface PresenceEventListener {
    void onEvent( String username, boolean event );
}
