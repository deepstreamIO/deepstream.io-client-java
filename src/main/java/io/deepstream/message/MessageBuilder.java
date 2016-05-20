package io.deepstream.message;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import io.deepstream.constants.Actions;
import io.deepstream.constants.Topic;
import io.deepstream.constants.Types;

public class MessageBuilder {

    static private final String MPS = Character.toString( '\u001f' );
    static private final String MS = Character.toString( '\u001e' );

    public static String getMsg( Topic topic, Actions action, String name, String data ) {
        return topic.toString() + MPS + action.toString() + MPS + name + MPS +  data + MS;
    }

    public static String getMsg( Topic topic, Actions action, String data ) {
        return topic.toString() + MPS + action.toString() + MPS + data + MS;
    }

    public static String getMsg( Topic topic, Actions action ) {
        return topic.toString() + MPS + action.toString() + MS;
    }

    /**
     * Converts a serializable value into its string-representation and adds
     * a flag that provides instructions on how to deserialize it.
     * @param {Mixed} value
     * @returns {String} string representation of the value
     */
    public static String typed( Object value ) {
        if( value instanceof String ) {
            return Types.STRING.toString() + value;
        }
        else if( value instanceof Number ) {
            return Types.NUMBER.toString() + value.toString();
        }
        else if( value instanceof Boolean ) {
            if( (Boolean)value == true ) {
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
}