package io.deepstream;

public interface UtilSingleNotifierCallback {
    void onSingleNotifierError( String name, Object error );
    void onSingleNotifierResponse( String name, Object data );
}