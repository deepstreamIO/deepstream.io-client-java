package io.deepstream;

import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

/**
 * A DeepstreamException is a RuntimeException since they mostly occur when recieving an out of sync message
 * from the server, or if an Ack has timed out. You can catch these via {@link DeepstreamClient#setRuntimeErrorHandler(DeepstreamRuntimeErrorHandler)}
 * which would stop the Error from being thrown.
 */
public class DeepstreamException extends RuntimeException {
    public Event event;
    public String message;
    public Topic topic;


    DeepstreamException(String message ) {
        super( message );
    }

    DeepstreamException(Topic topic, Event event, String message) {
        super( event + ": " + message );
        this.topic = topic;
        this.event = event;
        this.message = message;
    }
}
