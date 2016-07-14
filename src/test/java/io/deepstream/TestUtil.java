package io.deepstream;

class TestUtil {

    private static final char MPS =  '\u001f';
    private static final char MS = '\u001e';

    public static String replaceSeperators(String input ) {
        return input
                .replaceAll( "<UID>", "[a-z0-9]{8}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{12}")
                .replace( '|', MPS )
                .replace( '+', MS );
    }

}
