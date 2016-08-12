package io.deepstream;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

public enum Actions {
    ACK( "A" ),
    REDIRECT( "RED" ),
    CHALLENGE( "CH" ),
    CHALLENGE_RESPONSE( "CHR" ),
    READ( "R" ),
    CREATE( "C" ),
    UPDATE( "U" ),
    PATCH( "P" ),
    DELETE( "D" ),
    SUBSCRIBE( "S" ),
    UNSUBSCRIBE( "US" ),
    HAS( "H" ),
    SNAPSHOT( "SN" ),
    INVOKE( "I" ),
    SUBSCRIPTION_FOR_PATTERN_FOUND( "SP" ),
    SUBSCRIPTION_FOR_PATTERN_REMOVED( "SR" ),
    LISTEN( "L" ),
    UNLISTEN( "UL" ),
    PROVIDER_UPDATE( "PU" ),
    QUERY( "Q" ),
    CREATEORREAD( "CR" ),
    EVENT( "EVT" ),
    ERROR( "E" ),
    REQUEST( "REQ" ),
    RESPONSE( "RES" ),
    REJECTION( "REJ" );

    private String action;
    Actions( String action ) {
        this.action = action;
    }

    @Override
    public String toString() {
        return this.action;
    }

    private static final Map<String,Actions> lookup  = new HashMap<>();
    static {
        for( Actions s : EnumSet.allOf( Actions.class ) )
            lookup.put( s.toString(), s);
    }

    static Actions getAction( String action ) {
        return lookup.get( action );
    }
}