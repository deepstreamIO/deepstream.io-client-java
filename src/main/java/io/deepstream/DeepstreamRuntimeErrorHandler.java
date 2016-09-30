package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

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
     * @param topic The Topic the error occured on
     * @param event The Error Event
     * @param errorMessage The error message
     */
    @ObjectiveCName("onException:event:errorMessage:")
    void onException(Topic topic, Event event, String errorMessage);
}
