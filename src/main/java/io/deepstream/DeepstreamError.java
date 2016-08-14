package io.deepstream;

/**
 * DeepstreamErrors are used for expected error cases, such as during {@link RecordHandler#snapshot(String)} where the
 * result is either the data or a record not found error.
 * </br>
 * The DeepstreamError extends {@link Exception}
 */
public class DeepstreamError extends Exception {

    DeepstreamError(String error) {
        super(error);
    }
}
