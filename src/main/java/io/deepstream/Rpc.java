package io.deepstream;

import io.deepstream.constants.Actions;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.util.Map;


class Rpc {

    private String uid;
    private UtilAckTimeoutRegistry ackTimeoutRegistry;
    Map properties;
    IDeepstreamClient client;
    RpcResponseCallback callback;

    public Rpc( Map properties, IDeepstreamClient client, String uid, RpcResponseCallback callback ) {
        this.properties = properties;
        this.client = client;
        this.uid = uid;
        this.callback = callback;
        this.ackTimeoutRegistry = UtilAckTimeoutRegistry.getAckTimeoutRegistry( this.client );
        this.setTimeouts();
    }

    public void ack() {
        this.ackTimeoutRegistry.clear( Topic.RPC, Actions.REQUEST, this.uid );
    }

    public void respond( String data ) {
        Object convertedData = MessageParser.convertTyped( data, this.client );
        this.callback.onData( convertedData );
        this.clearTimeouts();
    }

    public void error(String err ) {
        this.callback.onError( err );
        this.clearTimeouts();
    }

    private void clearTimeouts() {
        this.ackTimeoutRegistry.clear( Topic.RPC, Actions.REQUEST, this.uid );
        this.ackTimeoutRegistry.clear( Topic.RPC, Actions.RESPONSE, this.uid );
    }

    private void setTimeouts() {
        int ackTimeoutTime = Integer.parseInt( (String) properties.get( "rpcAckTimeout" ) );
        this.ackTimeoutRegistry.add( Topic.RPC, Actions.REQUEST, this.uid, Event.ACK_TIMEOUT, ackTimeoutTime );

        int responseTimeoutTime = Integer.parseInt( (String) properties.get( "rpcResponseTimeout" ) );
        this.ackTimeoutRegistry.add( Topic.RPC, Actions.RESPONSE, this.uid, Event.RESPONSE_TIMEOUT, responseTimeoutTime );
    }
}
