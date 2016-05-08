package io.deepstream.constants;

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

    private String topic;
    Topic( String topic ) {
        this.topic = topic;
    }

    @Override
    public String toString() {
        return this.topic;
    }

    private static final Map<String,Topic> lookup  = new HashMap<String,Topic>();
    static {
        for( Topic s : EnumSet.allOf( Topic.class ) )
            lookup.put( s.toString(), s);
    }

    static public Topic getTopic( String topic ) {
        return lookup.get( topic );
    }
}
