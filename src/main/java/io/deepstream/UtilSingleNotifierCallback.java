package io.deepstream;

/**
 * TODO: Naming?
 */
interface UtilSingleNotifierCallback {
    void onSingleNotifierError( String name, DeepstreamException error );
    void onSingleNotifierResponse( String name, Object data );
}