package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import java.util.EnumSet;

/**
 * Provides all the different events that may occur. Most are related to errors for finer debugging and logging
 */
public enum Event {
    /**
     * To indicate a connection has not been authenticated longer than expected
     */
    UNAUTHENTICATED_CONNECTION_TIMEOUT,
    /**
     * To indicate a connection error was encountered
     */
    CONNECTION_ERROR,
    /**
     * To indicate the connection state was changed
     */
    CONNECTION_STATE_CHANGED,
    /**
     * To indicate an subscription ack has timed out
     */
    ACK_TIMEOUT,
    /**
     * To indicate a record read from deepstream has timed out
     */
    RESPONSE_TIMEOUT,
    /**
     * To indicate a record read from deepstream has timed out while attempting to read from cache
     */
    CACHE_RETRIEVAL_TIMEOUT,
    /**
     * To indicate a record read from deepstream has timed out while attempting to read from storage
     */
    STORAGE_RETRIEVAL_TIMEOUT,
    /**
     * To indicate a record delete timed out
     */
    DELETE_TIMEOUT,
    /**
     * To indicate the client has received a message that it didn't expect
     */
    UNSOLICITED_MESSAGE,
    /**
     * To indicate the client has received a message that can't be parsed
     */
    MESSAGE_PARSE_ERROR,
    /**
     * To indicate the client has received a record update that conflicts with the one
     * in cache
     */
    VERSION_EXISTS,
    /**
     * To indicate the client has attempted to perform an operation before authenticating
     */
    NOT_AUTHENTICATED,
    /**
     * To indicate the client has attempted to add a listen pattern twice
     */
    LISTENER_EXISTS,
    /**
     * To indicate the client has attempted to remove a pattern that doesn't exist
     */
    NOT_LISTENING,
    /**
     * To indicate the client has attempted to login with incorrect credentials too many times
     */
    TOO_MANY_AUTH_ATTEMPTS,
    /**
     * To indicate the client has attempted to perform an action a connection that has been closed
     */
    IS_CLOSED,
    /**
     * To indicate the client has attempted to get a snapshot of a record that doesn't exist
     */
    RECORD_NOT_FOUND,
    /**
     * To indicate the client has attempted to perform an action they are not permissioned too
     */
    MESSAGE_DENIED;

    @ObjectiveCName("getEvent:")
    static Event getEvent(String event ) {

        for( Event s : EnumSet.allOf( Event.class ) ) {
            if( s.name().equals( event ) ) {
                return s;
            }
        }
        return null;
    }
}
