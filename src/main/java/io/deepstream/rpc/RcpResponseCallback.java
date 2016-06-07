package io.deepstream.rpc;

public interface RcpResponseCallback {

    void onData( Object data );

    void onError( String err );
}