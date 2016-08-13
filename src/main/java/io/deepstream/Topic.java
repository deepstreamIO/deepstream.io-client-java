package io.deepstream;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

public enum Topic {
    /**
     * Connection topic, related to the first exchanges with server to validate connection
     */
    CONNECTION( "C" ),
    /**
     * Auth topic, related to the second exchange with server to authenticate
     * connection
     */
    AUTH( "A" ),
    /**
     * All generic errors arrive on this topic
     */
    ERROR( "X" ),
    /**
     * Event data is routed through this topic
     */
    EVENT( "E" ),
    /**
     * Record data is routed through this topic
     */
    RECORD( "R" ),
    /**
     * RPC data is routed through this topic
     */
    RPC("P");

    private static final Map<String,Topic> lookup  = new HashMap<String,Topic>();

    static {
        for( Topic s : EnumSet.allOf( Topic.class ) )
            lookup.put( s.toString(), s);
    }

    private String topic;

    Topic( String topic ) {
        this.topic = topic;
    }

    static Topic getTopic( String topic ) {
        return lookup.get( topic );
    }

    @Override
    public String toString() {
        return this.topic;
    }
}
