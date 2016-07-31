package io.deepstream;

/**
 * Called whenever you try to perform an action on a record that has been discarded/deleted. Retrieve a new instance of the
 * Record using {@link RecordHandler#getRecord(String)} to continue using it.
 */
public class DeepstreamRecordDestroyedException extends RuntimeException {

    private final String method;

    DeepstreamRecordDestroyedException(String method ) {
        this.method = method;
    }
}
