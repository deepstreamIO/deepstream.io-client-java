package io.deepstream;


public interface RpcRequested {

    void Call( Object data, RpcResponse response );
}
