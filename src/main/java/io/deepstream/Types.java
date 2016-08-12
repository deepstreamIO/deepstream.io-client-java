package io.deepstream;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

public enum Types {
    STRING( "S" ),
    OBJECT( "O" ),
    NUMBER( "N" ),
    NULL( "L" ),
    TRUE( "T" ),
    FALSE( "F" ),
    UNDEFINED( "U" );

    private static final Map<String,Types> lookup  = new HashMap<>();

    static {
        for( Types s : EnumSet.allOf( Types.class ) )
            lookup.put( s.toString(), s);
    }

    private String type;

    Types( String type ) {
        this.type = type;
    }

    static Types getType( char type ) {
        return lookup.get( type + "" );
    }

    @Override
    public String toString() {
        return this.type;
    }
}
