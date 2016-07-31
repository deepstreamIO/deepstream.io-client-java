package io.deepstream;

import io.deepstream.constants.*;

/**
 * Message is the internal representation of a message that is sent to/recieved from a deepstream
 * server
 */
class Message {
    public String raw;
    public Actions action;
    public Topic topic;
    public String[] data;

    /**
     * @param raw The raw data recieved
     * @param topic
     * @param action
     * @param data
     */
    public Message( String raw, Topic topic, Actions action, String[] data ) {
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
