package io.deepstream;

/**
 * TODO: Naming?
 */
public interface SingleNotifierCallback {
    void onSingleNotifierError( String name, DeepstreamException error );
    void onSingleNotifierResponse( String name, Object data );
}