package io.deepstream;

/**
 * The callback for an rpc that has been requested by the client
 */
public interface RpcResponseCallback {
    /**
     * Called when the rpc was completed successfully by another client that has provided the rpc via
     * {@link RpcHandler#provide(String, RpcRequestedListener)}
     * @param rpcName The rpc name
     * @param data The result data from the rpc
     */
    void onRpcSuccess(String rpcName, Object data);

    /**
     * Called when the rpc was completed unsuccessfully by another client that has provided the rpc via
     * {@link RpcHandler#provide(String, RpcRequestedListener)}
     * @param rpcName The rpc name
     */
    void onRpcError(String rpcName, Object error);
}