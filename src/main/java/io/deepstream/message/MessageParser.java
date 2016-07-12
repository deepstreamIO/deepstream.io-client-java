package io.deepstream.message;

import com.google.gson.Gson;
import io.deepstream.IDeepstreamClient;
import io.deepstream.constants.Actions;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;
import io.deepstream.constants.Types;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class MessageParser {

    static private final String MPS = Character.toString( '\u001f' );
    static private final String MS = Character.toString( '\u001e' );

     /**
     * Main interface method. Receives a raw message
     * string, containing one or more messages
     * and returns an array of parsed message objects
     * or null for invalid messages
     */
    static List<Message> parse( String message, Connection connection ) {
        List<Message> messages = new ArrayList();
        String[] rawMessages = message.split( MS );
        for( short i=0; i < rawMessages.length; i++ ) {
            messages.add( parseMessage( rawMessages[ i ] ) );
        }
        return messages;
    }

    static private Message parseMessage( String message ) {
        String[] parts = message.split( MPS );

        if( parts.length < 2 ) {
            throw new Error( Event.MESSAGE_PARSE_ERROR.name() + " Insufficient Parts" );
        }

        if( Topic.getTopic( parts[ 0 ] ) == null ) {
            throw new Error( Event.MESSAGE_PARSE_ERROR.name() + " Incorrect Type " + parts[ 0 ]  );
        }

        if( Actions.getAction( parts[ 1 ] ) == null ) {
            throw new Error( Event.MESSAGE_PARSE_ERROR.name() + " Incorrect Action " + parts[ 1 ] );
        }

        return new Message( message, Topic.getTopic( parts[ 0 ] ), Actions.getAction( parts[ 1 ] ), Arrays.copyOfRange( parts, 2, parts.length ) );
    }

    public static Object convertTyped( String value, IDeepstreamClient client ) {

        char type = value.charAt(0);

        if( Types.getType( type ) == Types.STRING ) {
            return value.substring( 1 );
        }
        else if( Types.getType( type ) == Types.NULL ) {
            return null;
        }
        else if( Types.getType( type ) == Types.NUMBER ) {
            return Float.parseFloat( value.substring( 1 ) );
        }
        else if( Types.getType( type ) == Types.TRUE ) {
            return true;
        }
        else if( Types.getType( type ) == Types.FALSE ) {
            return false;
        }
        else if( Types.getType( type ) == Types.OBJECT ) {
            return new Gson().fromJson( value.substring( 1 ), Object.class );
        }
        else if( Types.getType( type ) == Types.UNDEFINED ) {
            // Undefined isn't a thing in Java..
        }

        client.onError( Topic.ERROR, Event.MESSAGE_PARSE_ERROR, "UNKNOWN_TYPE (" + value + ")" );
        return null;
    }

}