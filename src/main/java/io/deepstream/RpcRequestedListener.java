package io.deepstream;

/**
 * Listener for any rpc requests recieved from the server
 */
public interface RpcRequestedListener {
    /**
     * This listener will be invoked whenever the client recieves an rpc request from the server, and will be able
     * to respond via {@link RpcRequest#send(Object)} or {@link RpcRequest#reject()}
     * @param rpcName The name of the rpc being requested
     * @param data The data the request was made with
     * @param response The {@link RpcRequest} to respond to the request with
     */
    void onRPCRequested(String rpcName, Object data, RpcRequest response);
}
