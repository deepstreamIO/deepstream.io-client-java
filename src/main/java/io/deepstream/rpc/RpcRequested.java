package io.deepstream.rpc;


public interface RpcRequested {

    void Call( Object data, RpcResponse response );
}
