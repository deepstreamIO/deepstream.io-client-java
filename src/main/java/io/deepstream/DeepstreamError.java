package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

/**
 * DeepstreamErrors are used for expected error cases, such as during {@link RecordHandler#snapshot(String)} where the
 * result is either the data or a record not found error.
 * </br>
 * The DeepstreamError extends {@link Exception}
 */
public class DeepstreamError extends Exception {

	@ObjectiveCName("init:")
    DeepstreamError(String error) {
        super(error);
    }
}
