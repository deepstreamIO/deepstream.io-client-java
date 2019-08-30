package io.deepstream;
import com.google.protobuf.MessageLite;

/**
 * Creates a deepstream message string, based on the
 * provided parameters
 */
class MessageBuilder {
    byte[] getMessage (MessageLite msg) {
        return msg.toByteArray();
    }
}