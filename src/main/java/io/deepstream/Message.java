package io.deepstream;

import io.deepstream.constants.*;

/**
 * Message is the internal representation of a message that is sent to/recieved from a deepstream
 * server
 */
class Message {
    String raw;
    Actions action;
    Topic topic;
    String[] data;

    /**
     * @param raw The raw data recieved
     * @param topic The message topic
     * @param action The message action
     * @param data The message data, as an array
     */
    Message( String raw, Topic topic, Actions action, String[] data ) {
        this.raw = raw;
        this.topic = topic;
        this.action = action;
        this.data = data;
    }

    @Override
    public String toString() {
        return this.raw;
    }
}
