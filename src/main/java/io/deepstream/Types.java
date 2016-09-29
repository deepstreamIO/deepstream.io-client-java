package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;


enum Types {
    /**
     * A string representation.
     * Example: SDog -> "Dog"
     */
    STRING( "S" ),
    /**
     * A JsonElement representation.
     * Example: O{"type":"Dog"} -> JsonElement
     */
    OBJECT( "O" ),
    /**
     * A number representation.
     * Example: N15-> 15
     */
    NUMBER( "N" ),
    /**
     * Null representation
     * Example: L -> null
     */
    NULL( "L" ),
    /**
     * Boolean true representation
     * Example: T -> true
     */
    TRUE( "T" ),
    /**
     * Boolean False representation
     * Example: F -> false
     */
    FALSE( "F" ),
    /**
     * Undefined representation
     * Example: U -> ...does not exist in java..
     */
    UNDEFINED( "U" );

    private static final Map<String,Types> lookup  = new HashMap<>();

    static {
        for( Types s : EnumSet.allOf( Types.class ) )
            lookup.put( s.toString(), s);
    }

    private String type;

    @ObjectiveCName("init:")
    Types( String type ) {
        this.type = type;
    }

    @ObjectiveCName("getType:")
    static Types getType( char type ) {
        return lookup.get( type + "" );
    }

    @Override
    public String toString() {
        return this.type;
    }
}
