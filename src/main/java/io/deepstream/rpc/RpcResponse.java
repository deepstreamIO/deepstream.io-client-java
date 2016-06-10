package io.deepstream.rpc;


import io.deepstream.DeepstreamException;
import io.deepstream.IConnection;
import io.deepstream.constants.Actions;
import io.deepstream.constants.Topic;
import io.deepstream.message.MessageBuilder;

public class RpcResponse {

    private IConnection connection;
    private String name;
    private String correlationId;
    private boolean isAcknowledged;
    private boolean isComplete;

    public RpcResponse( IConnection connection, String name, String correlationId ) {
        this.connection = connection;
        this.name = name;
        this.correlationId = correlationId;
        this.isAcknowledged = false;
        this.isComplete = false;
    }

    public void ack() {
        if( this.isAcknowledged == false ) {
            this.connection.sendMsg( Topic.RPC, Actions.ACK, new String[] { this.name, this.correlationId } );
        }
    }

    public void reject() {
        this.isComplete = true;
        this.isAcknowledged = true;
        this.connection.sendMsg( Topic.RPC, Actions.REJECTION, new String[] { this.name, this.correlationId } );
    }

    public void send( Object data ) {
        if( this.isComplete == true ) {
            throw new DeepstreamException( "Rpc " + this.name + " already completed" );
        }
        String typedData = MessageBuilder.typed( data );
        this.connection.sendMsg( Topic.RPC, Actions.RESPONSE, new String[] {
                this.name, this.correlationId, typedData
        } );
        this.isComplete = true;
    }
}
