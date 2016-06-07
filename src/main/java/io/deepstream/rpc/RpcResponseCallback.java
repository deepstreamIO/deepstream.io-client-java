package io.deepstream.rpc;

public interface RpcResponseCallback {

    void onData( Object data );

    void onError( String err );
}