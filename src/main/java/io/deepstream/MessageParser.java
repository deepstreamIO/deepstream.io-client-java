package io.deepstream;

import com.google.protobuf.InvalidProtocolBufferException;

import com.google.protobuf.MessageLite;
import io.deepstream.protobuf.General;
import io.deepstream.protobuf.Connection;
import io.deepstream.protobuf.Auth;
import io.deepstream.protobuf.Event;

import java.util.ArrayList;
import java.util.List;

/**
 * Parses ASCII control character seperated
 * message strings into {@link Message}
 */
class MessageParser {
    /**
     * Main interface method. Receives a raw message
     * string, containing one or more messages
     * and returns an array of parsed message objects
     * or null for invalid messages
     */
    static List<MessageLite> parse(byte[] message, DeepstreamClientAbstract client ) {
        List<MessageLite> messages = new ArrayList<>();
        try {
            General.Message msg = General.Message.parseFrom(message);
            switch (msg.getTopicValue()) {
                case General.TOPIC.CONNECTION_VALUE:
                    messages.add(Connection.ConnectionMessage.parseFrom(msg.getMessage()));
                    break;
                case General.TOPIC.AUTH_VALUE:
                    messages.add(Auth.AuthMessage.parseFrom(msg.getMessage()));
                    break;
                case General.TOPIC.EVENT_VALUE:
                    messages.add(Event.EventMessage.parseFrom(msg.getMessage()));
                    break;

            }
        } catch (InvalidProtocolBufferException e) {
            e.printStackTrace();
        }
        return messages;
    }
}