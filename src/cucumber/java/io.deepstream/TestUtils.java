package io.deepstream;

class TestUtils {

    private static final char MPS =  '\u001f';
    private static final char MS = '\u001e';

    public static String matchMessage( String input ) {
        return input
                .replace( MPS, '|' )
                .replace( MS, '+' );
    }

    public static String convertChars( String input ) {
        return input
                .replace( "{", "\\{" )
                .replace( "}", "\\}" )
                .replaceAll( "<UID>", "[a-z0-9]{8}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{12}")
                .replaceAll( "<FIRST_SERVER_URL>", "localhost:[0-9]{4}")
                .replace( '|', MPS )
                .replace( '+', MS );
    }
}
