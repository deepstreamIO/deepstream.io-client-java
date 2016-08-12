package io.deepstream;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

public enum Topic {
    CONNECTION( "C" ),
    AUTH( "A" ),
    ERROR( "X" ),
    EVENT( "E" ),
    RECORD( "R" ),
    RPC( "P" ),
    WEBRTC( "W" ),
    PRIVATE( "PRIVATE/" );

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
