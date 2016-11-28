package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

/**
 * Actions provide the intent of a message sent. A user of this sdk will only need to access these
 * during error messages, such as via the {@link DeepstreamException}
 */
public enum Actions {
    /**
     * Error action to indicate something on the server did not go as expected
     */
    ERROR("E"),
    /**
     * A heartbeat ping from server
     */
    PING( "PI" ),
    /**
     * An heartbeat pong to server
     */
    PONG( "PO" ),
    /**
     * An acknowledgement from serer
     */
    ACK( "A" ),
    /**
     * A connection redirect to allow client to connect to other deepstream
     * for load balancing
     */
    REDIRECT( "RED" ),
    /**
     * A connection challenge
     */
    CHALLENGE( "CH" ),
    /**
     * Connection challenge response
     */
    CHALLENGE_RESPONSE( "CHR" ),
    /**
     * A request or response containing record data for a snapshot or subscription
     */
    READ( "R" ),
    /**
     * A create to indicate record can be created if it doesn't exist
     */
    CREATE( "C" ),
    /**
     * A combination of both the create and read actions
     */
    CREATEORREAD("CR"),
    /**
     * An update, meaning all data in record has been updated
     */
    UPDATE( "U" ),
    /**
     * A path, meaning a specific part of data under a path has been
     * updated
     */
    PATCH( "P" ),
    /**
     * Delete a record / be informed a record has been deleted
     */
    DELETE( "D" ),
    /**
     * Used to subscribe to most things, including events, records
     * ( although records use CR ) and providing rpcs
     */
    SUBSCRIBE( "S" ),
    /**
     * Used to unsubscribe to anything that was previously subscribed to
     */
    UNSUBSCRIBE( "US" ),
    /**
     * Action to enquire if record actually exists on deepstream
     */
    HAS( "H" ),
    /**
     * Ask for the current state of a record
     */
    SNAPSHOT( "SN" ),
    /**
     * Used to inform the client listener that it has the opportunity to provide the data
     * for a event or record subscription
     */
    SUBSCRIPTION_FOR_PATTERN_FOUND( "SP" ),
    /**
     * Used to inform listener that it it is no longer required to provide the data for a
     * event or record subscription
     */
    SUBSCRIPTION_FOR_PATTERN_REMOVED( "SR" ),
    /**
     * Used to indicate if a record has a provider currently providing it data
     */
    SUBSCRIPTION_HAS_PROVIDER("SH"),
    /**
     * Inform the server that it the client is willing to provide any subscription matching
     * a pattern
     */
    LISTEN( "L" ),
    /**
     * Inform the server that it the client is no longer willing to provide any subscription
     * matching a pattern
     */
    UNLISTEN( "UL" ),
    /**
     * Inform the server the provider is willing to provide the subscription
     */
    LISTEN_ACCEPT("LA"),
    /**
     * Inform the server the provider is not willing to provide the subscription
     */
    LISTEN_REJECT("LR"),
    /**
     * Inform the client a remote event has occured
     */
    EVENT( "EVT" ),
    /**
     * A request to the server, used for RPC, authentication and connection
     */
    REQUEST( "REQ" ),
    /**
     * A response from the server for a request
     */
    RESPONSE( "RES" ),
    /**
     * Used to reject RPC requests
     */
    REJECTION( "REJ" ),
    /**
     * Called when a user logs in
     */
    PRESENCE_JOIN( "PNJ" ),
    /**
     * Called when a user logs out
     */
    PRESENCE_LEAVE( "PNL" ),
    /**
     * Used to query for clients
     */
    QUERY( "Q" );

    private static final Map<String,Actions> lookup  = new HashMap<>();

    static {
        for( Actions s : EnumSet.allOf( Actions.class ) )
            lookup.put( s.toString(), s);
    }

    private String action;

    @ObjectiveCName("init:")
    Actions( String action ) {
        this.action = action;
    }

    @ObjectiveCName("getAction:")
    static Actions getAction( String action ) {
        return lookup.get( action );
    }

    @Override
    public String toString() {
        return this.action;
    }
}