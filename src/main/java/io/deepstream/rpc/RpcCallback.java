package io.deepstream.rpc;


public interface RpcCallback {

    void Call( Object data, RpcResponse response );
}
