package io.deepstream;

import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

public class DeepstreamException extends RuntimeException {
    public Event event;
    public String message;
    public Topic topic;


    public DeepstreamException(String message ) {
        super( message );
    }

    public DeepstreamException(Topic topic, Event event, String message) {
        super( event + ": " + message );
        this.topic = topic;
        this.event = event;
        this.message = message;
    }
}
