package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

class Rpc implements UtilTimeoutListener {

    private final String uid;
    private final UtilAckTimeoutRegistry ackTimeoutRegistry;
    private final DeepstreamConfig deepstreamConfig;
    private final DeepstreamClientAbstract client;
    private final RpcHandler.RpcResponseCallback callback;
    private final String rpcName;

    /**
     * An RPC represents a single remote procedure Request made from the client to the server.
     * It's main function is to encapsulate the logic around timeouts and to convert the
     * incoming response data
     *
     * @param deepstreamConfig The deepstreamConfig the client was created with
     * @param client The deepstream client
     * @param rpcName The rpc name
     * @param uid The unique RPC identifier
     * @param callback The callback when an RPC has been completed
     */
    @ObjectiveCName("init:client:rpcName:uid:callback:")
    Rpc(DeepstreamConfig deepstreamConfig, DeepstreamClientAbstract client, String rpcName, String uid, RpcHandler.RpcResponseCallback callback) {
        this.deepstreamConfig = deepstreamConfig;
        this.client = client;
        this.rpcName = rpcName;
        this.uid = uid;
        this.callback = callback;
        this.ackTimeoutRegistry = client.getAckTimeoutRegistry();
        this.setTimeouts();
    }

    /**
     * Called once an ack message is received from the server.<br/>
     */
    void ack() {
        this.ackTimeoutRegistry.clear( Topic.RPC, Actions.REQUEST, this.uid );
    }

    /**
     * Called once a response message is received from the server.
     * Converts the typed data and completes the request.
     * @param rpcName The rpc name
     * @param data The data received from the server
     */
    @ObjectiveCName("respond:data:")
    void respond( String rpcName, String data ) {
        Object convertedData = MessageParser.convertTyped( data, this.client );
        this.callback.onRpcSuccess(rpcName, convertedData);
        this.clearTimeouts();
    }

    /**
     * Callback for error messages received from the server. Once
     * an error is received the request is considered completed. Even
     * if a response arrives later on it will be ignored / cause an
     * UNSOLICITED_MESSAGE error
     * @param rpcName The rpc name
     * @param err The errorMessage received from the server
     */
    @ObjectiveCName("error:err:")
    void error( String rpcName, String err ) {
        this.callback.onRpcError(rpcName, err);
        this.clearTimeouts();
    }

    @Override
    @ObjectiveCName("onTimeout:action:event:name:")
    public void onTimeout(Topic topic, Actions action, Event event, String name) {
        this.error( this.rpcName, event.toString() );
    }

    private void clearTimeouts() {
        this.ackTimeoutRegistry.clear( Topic.RPC, Actions.REQUEST, this.uid );
        this.ackTimeoutRegistry.clear( Topic.RPC, Actions.RESPONSE, this.uid );
    }

    private void setTimeouts() {
        this.ackTimeoutRegistry.add(Topic.RPC, Actions.REQUEST, this.uid, Event.ACK_TIMEOUT, deepstreamConfig.getRpcAckTimeout());
        this.ackTimeoutRegistry.add(Topic.RPC, Actions.RESPONSE, this.uid, Event.RESPONSE_TIMEOUT, this, deepstreamConfig.getRpcResponseTimeout());
    }
}
