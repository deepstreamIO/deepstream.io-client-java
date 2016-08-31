package io.deepstream;

/**
 * Provides all the different connection states a deepstream client can go through its lifetime
 */
public enum ConnectionState {
    /**
     * Connection is closed, usually due to closing the client or getting multiple authentication rejects
     * from server
     */
    CLOSED,
    /**
     * Client is attempting to connect to a server
     */
    AWAITING_CONNECTION,
    /**
     * Client has connected to a server and is now enquiring if it should open a normal connection or be redirect
     * to another server first
     */
    CHALLENGING,
    /**
     * Client is waiting for user to login in order to authenticate connection with server
     */
    AWAITING_AUTHENTICATION,
    /**
     * Client has sent authentication to server and is waiting for a response
     */
    AUTHENTICATING,
    /**
     * Client connection is up and running
     */
    OPEN,
    /**
     * An error has occured on the connection, usually due to an recoverable connection drop or if no
     * server exists to connect to
     */
    ERROR,
    /**
     * Client is attempting to reconnect due to connection drop
     */
    RECONNECTING
}
