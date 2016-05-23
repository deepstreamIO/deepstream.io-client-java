package io.deepstream.util;

/**
 * Created by Alex on 23/05/2016.
 */
public class Util {

    private static final char MPS =  '\u001f';
    private static final char MS = '\u001e';

    public static String matchMessage( String input ) {
        return input
                .replace( MPS, '|' )
                .replace( MS, '+' );
    }

    public static String convertChars( String input ) {
        return input
                .replace( '|', MPS )
                .replace( '+', MS );
    }
}
