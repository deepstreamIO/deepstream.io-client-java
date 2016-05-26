package io.deepstream;

import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

public class DeepstreamException extends RuntimeException {
    public DeepstreamException( String message ) {
        super( message );
    }

    public DeepstreamException(Topic topic, Event event, String message) {
        String errorMsg = event + ": " + message;
        errorMsg += " (" + topic + ")";
        new DeepstreamException( errorMsg );
    }
}
