package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

/**
 * Message is the internal representation of a message that is sent to received from a deepstream
 * server
 */
class Message {
    final String raw;
    final String[] data;
    final Actions action;
    final Topic topic;

    /**
     * @param raw The raw data recieved
     * @param topic The message topic
     * @param action The message action
     * @param data The message data, as an array
     */
    @ObjectiveCName("init:topic:action:data:")
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
