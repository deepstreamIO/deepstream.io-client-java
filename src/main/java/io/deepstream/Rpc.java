package io.deepstream;

import io.deepstream.constants.Actions;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.util.Map;


class Rpc implements UtilTimeoutListener {

    private final String uid;
    private final UtilAckTimeoutRegistry ackTimeoutRegistry;
    private final Map properties;
    private final DeepstreamClientAbstract client;
    private final RpcResponseCallback callback;
    private final String rpcName;

    /**
     * An RPC represents a single remote procedure Request made from the client to the server.
     * It's main function is to encapsulate the logic around timeouts and to convert the
     * incoming response data
     *
     * @param options The options the client was created with
     * @param client The deepstream client
     * @param rpcName The rpc name
     * @param uid The unique RPC identifier
     * @param callback The callback when an RPC has been completed
     */
    Rpc(Map options, DeepstreamClientAbstract client, String rpcName, String uid, RpcResponseCallback callback ) {
        this.properties = options;
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
    void error( String rpcName, String err ) {
        this.callback.onRpcError(rpcName, err);
        this.clearTimeouts();
    }

    @Override
    public void onTimeout(Topic topic, Actions action, Event event, String name) {
        this.error( this.rpcName, event.toString() );
    }

    private void clearTimeouts() {
        this.ackTimeoutRegistry.clear( Topic.RPC, Actions.REQUEST, this.uid );
        this.ackTimeoutRegistry.clear( Topic.RPC, Actions.RESPONSE, this.uid );
    }

    private void setTimeouts() {
        int ackTimeoutTime = Integer.parseInt( (String) properties.get( "rpcAckTimeout" ) );
        this.ackTimeoutRegistry.add( Topic.RPC, Actions.REQUEST, this.uid, Event.ACK_TIMEOUT, ackTimeoutTime );

        int responseTimeoutTime = Integer.parseInt( (String) properties.get( "rpcResponseTimeout" ) );
        this.ackTimeoutRegistry.add( Topic.RPC, Actions.RESPONSE, this.uid, Event.RESPONSE_TIMEOUT, this, responseTimeoutTime );
    }
}
