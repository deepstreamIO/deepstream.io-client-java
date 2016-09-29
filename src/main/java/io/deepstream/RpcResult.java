package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

/**
 * RpcResult provides you access to the response state
 * of a rpc request called via {@link RpcHandler#make(String, Object)}
 */
public class RpcResult {

    private boolean success;
    private Object data;

    /**
     * This object gives you access to the rpc response state
     * to respond to a request
     * @param success true the rpc completed succesfully
     * @param data the data returned by the request
     */
    @ObjectiveCName("init:data:")
    RpcResult(boolean success, Object data) {
        this.success = success;
        this.data = data;
    }

    /**
     * Whether or not the RPC completed
     * @return true if the Request was completed successfully
     */
    public boolean success() {
        return this.success;
    }

    /**
     * The data returned by the RPC. If {@link RpcResult#success()} is true the resulting
     * data from your rpc, if false data associated with why it failed.
     * @return the object the provider returned
     */
    public Object getData() {
        return this.data;
    }
}
