package io.deepstream;

import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

/**
 * The expectations would be for java clients to implement this and add it
 * via the *setDeepstreamRuntimeErrorHandler* handler in order to catch all
 * runtime errors that can occur without requiring to listening to it on seperate
 * threads.
 *
 * IMPORTANT: Errors that are specific to a request, e.g. a RPC
 * timing out or a record not being permissioned are either passed directly
 * to the method that requested them or will be caught on a more granular listener.
 */
public interface DeepstreamRuntimeErrorHandler {

    /**
     * Triggered whenever a runtime error occurs ( mostly async such as TimeOuts or MergeConflicts ).
     * Recieves a topic to indicate if it was e.g. RPC, event and a english error message to simplify
     * debugging purposes.
     * @param topic
     * @param event
     * @param msg
     */
    void onException(Topic topic, Event event, String msg);
}
