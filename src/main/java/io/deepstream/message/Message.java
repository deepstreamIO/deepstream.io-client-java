package io.deepstream.message;

import io.deepstream.constants.*;

public class Message {
    public String raw;
    public Actions action;
    public Topic topic;
    public String[] data;

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
