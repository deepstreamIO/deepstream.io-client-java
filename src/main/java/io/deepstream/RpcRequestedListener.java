package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

/**
 * Listener for any rpc requests recieved from the server
 */
public interface RpcRequestedListener {
    /**
     * This listener will be invoked whenever the client recieves an rpc request from the server, and will be able
     * to respond via {@link RpcResponse#send(Object)} or {@link RpcResponse#reject()}
     * @param rpcName The name of the rpc being requested
     * @param data The data the request was made with
     * @param response The {@link RpcResponse} to respond to the request with
     */
    @ObjectiveCName("onRPCRequested:data:response:")
    void onRPCRequested(String rpcName, Object data, RpcResponse response);
}
