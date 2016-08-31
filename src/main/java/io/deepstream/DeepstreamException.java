package io.deepstream;

/**
 * A DeepstreamException is a RuntimeException since they mostly occur when receiving an out of sync message
 * from the server, or if an Ack has timed out. You can catch these via {@link DeepstreamClient#setRuntimeErrorHandler(DeepstreamRuntimeErrorHandler)}
 * which would stop the Error from being thrown.
 * </br>
 * The DeepstreamException extends {@link RuntimeException}
 */
public class DeepstreamException extends RuntimeException {
    /**
     * The Topic the event occured on, such as {@link Topic#RECORD}
     */
    public Topic topic;
    /**
     * The exception event, such as {@link Event#ACK_TIMEOUT}
     */
    public Event event;
    /**
     * The exception message, explaning the issue in english for logging
     * purposes
     */
    public String message;


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
