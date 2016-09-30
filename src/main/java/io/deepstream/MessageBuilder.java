package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

/**
 * Creates a deepstream message string, based on the
 * provided parameters
 */
class MessageBuilder {

    static private final String MPS = Character.toString( '\u001f' );
    static private final String MS = Character.toString( '\u001e' );

    @ObjectiveCName("getMsg:action:name:data:")
    public static String getMsg( Topic topic, Actions action, String name, String data ) {
        return topic.toString() + MPS + action.toString() + MPS + name + MPS +  data + MS;
    }

    @ObjectiveCName("getMsg:action:data:")
    public static String getMsg( Topic topic, Actions action, String data ) {
        return topic.toString() + MPS + action.toString() + MPS +  data + MS;
    }

    @ObjectiveCName("getMsg:action:dataArray:")
    public static String getMsg( Topic topic, Actions action, String[] data ) {
        return topic.toString() + MPS + action.toString() + MPS + join( data) + MS;
    }

    @ObjectiveCName("getMsg:action:")
    public static String getMsg( Topic topic, Actions action ) {
        return topic.toString() + MPS + action.toString() + MS;
    }

    /**
     * Converts a serializable value into its string-representation and adds
     * a flag that provides instructions on how to deserialize it.
     * @param value The value to serialised
     * @return string representation of the value
     */
    @ObjectiveCName("typed:")
    public static String typed( Object value ) {
        if( value instanceof String ) {
            return Types.STRING.toString() + value;
        }
        else if( value instanceof Number ) {
            return Types.NUMBER.toString() + value.toString();
        }
        else if( value instanceof Boolean ) {
            if((Boolean) value) {
                return Types.TRUE.toString();
            } else {
                return Types.FALSE.toString();
            }
        }
        else if( value == null ) {
            return Types.NULL.toString();
        }
        else {
            Gson gson = new GsonBuilder().enableComplexMapKeySerialization().create();
            return Types.OBJECT.toString() + gson.toJson( value );
        }
    }

    /**
     * Joins all the elements of the list together with the given sequence
     *
     * @param list A list of messages
     * @return A string representation of messages joined together
     */
    @ObjectiveCName("join:")
    private static String join(String[] list) {

        StringBuilder sb = new StringBuilder();
        for ( int i = 0; i < list.length; i++ ) {
            if( i == list.length - 1 ) {
                sb.append( list[ i ] );
            } else {
                sb.append( list[ i ] );
                sb.append(MessageBuilder.MPS);
            }
        }
        return sb.toString();
    }
}