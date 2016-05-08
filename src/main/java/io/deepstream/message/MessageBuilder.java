package io.deepstream.message;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.sun.deploy.util.StringUtils;
import io.deepstream.constants.*;

class MessageBuilder {

    static private final String MPS = Character.toString( '\u001f' );
    static private final String MS = Character.toString( '\u001e' );

    static String getMsg( Topic topic, Actions action ) {
        return topic.toString() + MPS + action.toString() + MS;
    }

    static String getMsg( Topic topic, Actions action, String data ) {
        return topic.toString() + MPS + action.toString() + MPS + data + MS;
    }

    static String getMsg( Message message ) {
        return getMsg( message.topic, message.action, Arrays.asList( message.data ) );
    }

    /**
     * Creates a deepstream message string, based on the
     * provided parameters
     *
     * @param   {String} topic  One of CONSTANTS.TOPIC
     * @param   {String} action One of CONSTANTS.ACTIONS
     * @param   {Array} data An array of strings or JSON-serializable objects
     *
     * @returns {String} deepstream message string
     */
    static String getMsg( Topic topic, Actions action, List data ) {
        ArrayList<String> sendData = new ArrayList();

        if( data != null ) {
            for( short i = 0; i < data.size(); i++ ) {
                if( data.get( i ) instanceof Object ) {
                    //sendData.add();
                } else {
                    sendData.add( data.get( i ).toString() );
                }
            }
        }

        return topic.toString() + MPS + action.toString() + StringUtils.join( sendData, MPS ) + MS;
    }

    /**
     * Converts a serializable value into its string-representation and adds
     * a flag that provides instructions on how to deserialize it.
     * @param {Mixed} value
     * @returns {String} string representation of the value
     */
    static String typed( String value ) {
        return "S" + value;
    }

    /**
     * Converts a serializable value into its string-representation and adds
     * a flag that provides instructions on how to deserialize it.
     * @param {Mixed} value
     * @returns {String} string representation of the value
     */
    static String typed( Number value ) {
        return "N" + value;
    }

    /**
     * Converts a serializable value into its string-representation and adds
     * a flag that provides instructions on how to deserialize it.
     * @param {Mixed} value
     * @returns {String} string representation of the value
     */
    static String typed( Boolean value ) {
        if( value == false ) {
            return Types.FALSE.toString();
        } else {
            return Types.TRUE.toString();
        }
    }

/*    *//**
     * Converts a serializable value into its string-representation and adds
     * a flag that provides instructions on how to deserialize it.
     * @param {Mixed} value
     * @returns {String} string representation of the value
     *//*
    static String typed( NullType ) {
        return Types.NULL.toString();
    }*/
}