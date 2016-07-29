package io.deepstream;

public interface RpcResponseCallback {
    void onData( Object data );
    void onError( String err );
}