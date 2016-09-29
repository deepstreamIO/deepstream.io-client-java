package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import com.google.gson.Gson;
import com.google.gson.JsonElement;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Parses ASCII control character seperated
 * message strings into {@link Message}
 */
class MessageParser {

    static private final String MPS = Character.toString( '\u001f' );
    static private final String MS = Character.toString( '\u001e' );

     /**
     * Main interface method. Receives a raw message
     * string, containing one or more messages
     * and returns an array of parsed message objects
     * or null for invalid messages
     */
    @ObjectiveCName("parse:client:")
    static List<Message> parse( String message, DeepstreamClientAbstract client ) {
        List<Message> messages = new ArrayList<>();
        String[] rawMessages = message.split( MS );
        Message parsedMessage;
        for (String rawMessage : rawMessages) {
            parsedMessage = parseMessage(rawMessage, client);
            if (parsedMessage != null) {
                messages.add(parsedMessage);
            }
        }
        return messages;
    }

    /**
     * Parses an individual message (as oposed to a
     * block of multiple messages as is processed by {@link MessageParser#parse(String, DeepstreamClientAbstract)})
     * @param message The message parse
     * @param client The deepstream client to notify if errors occur
     * @return The {@link Message} object that represents the message string
     */
    @ObjectiveCName("parseMessage:client:")
    static Message parseMessage( String message, DeepstreamClientAbstract client ) {
        String[] parts = message.split( MPS );

        if( parts.length < 2 ) {
            client.onError( null, Event.MESSAGE_PARSE_ERROR, "Insufficient message parts" );
            return null;
        }

        if( Topic.getTopic( parts[ 0 ] ) == null ) {
            client.onError( null, Event.MESSAGE_PARSE_ERROR, "Received message for unknown topic " + parts[ 0 ] );
            return null;
        }

        if( Actions.getAction( parts[ 1 ] ) == null ) {
            client.onError( null, Event.MESSAGE_PARSE_ERROR, "Unknown action " + parts[ 1 ] );
            return null;
        }

        return new Message( message, Topic.getTopic( parts[ 0 ] ), Actions.getAction( parts[ 1 ] ), Arrays.copyOfRange( parts, 2, parts.length ) );
    }

    /**
     * Deserializes values created by {@link MessageBuilder#typed(Object)} to
     * their original format
     *
     * @param value The value to deserialise
     * @param client The deepstream client to notify if errors occur
     * @return The object the value represented
     */
    @ObjectiveCName("convertTyped:client:")
    static Object convertTyped( String value, DeepstreamClientAbstract client ) {

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
            return parseObject( value.substring( 1 ) );
        }
        else if( Types.getType( type ) == Types.UNDEFINED ) {
            // Undefined isn't a thing in Java..
        }

        client.onError( Topic.ERROR, Event.MESSAGE_PARSE_ERROR, "UNKNOWN_TYPE (" + value + ")" );
        return null;
    }

    @ObjectiveCName("parseObject:")
    static Object parseObject(String value) {
        return new Gson().fromJson( value, JsonElement.class );
    }
}