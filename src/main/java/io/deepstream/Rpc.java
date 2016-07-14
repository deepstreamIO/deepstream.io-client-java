package io.deepstream;

import io.deepstream.constants.Actions;
import io.deepstream.constants.Topic;

import java.util.Map;


class Rpc {

    private UtilAckTimeoutRegistry ackTimeoutRegistry;
    Map properties;
    IDeepstreamClient client;
    RpcResponseCallback callback;

    public Rpc(Map properties, IDeepstreamClient client, RpcResponseCallback callback ) {
        this.properties = properties;
        this.client = client;
        this.callback = callback;
        this.ackTimeoutRegistry = UtilAckTimeoutRegistry.getAckTimeoutRegistry( this.client );
        this.setTimeouts();
    }

    public void ack() {
//        this.ackTimeout.cancel();
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
//        this.ackTimeoutRegistry.clear( );
//        this.responseTimeout.cancel();
    }

    private void setTimeouts() {
        int ackTimeoutTime = Integer.parseInt( (String) properties.get( "rpcAckTimeout" ) );
        int responseTimeoutTime = Integer.parseInt( (String) properties.get( "rpcResponseTimeout" ) );

        //Event.ACK_TIMEOUT
        this.ackTimeoutRegistry.add( Topic.RPC, Actions.REQUEST, "", ackTimeoutTime );

        //RESPONSE_TIMEOUT
        this.ackTimeoutRegistry.add( Topic.RPC, Actions.REQUEST, "", responseTimeoutTime );
    }
}
