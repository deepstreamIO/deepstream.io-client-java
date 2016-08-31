package io.deepstream;

/**
 * TODO: Remove somehow?
 */
interface IConnection {
    void sendMsg( Topic topic, Actions action, String[] data );
    void send( String message );
}
