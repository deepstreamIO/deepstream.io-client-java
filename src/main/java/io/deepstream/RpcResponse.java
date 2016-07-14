package io.deepstream;


import io.deepstream.constants.Actions;
import io.deepstream.constants.Topic;

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
        this.ack();
    }

    private void ack() {
        if( this.isAcknowledged == false ) {
            this.connection.sendMsg( Topic.RPC, Actions.ACK, new String[] { this.name, this.correlationId } );
            this.isAcknowledged = true;
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

    public void error( String err ) {
        this.isComplete = true;
        this.isAcknowledged = true;
        this.connection.sendMsg( Topic.RPC, Actions.ERROR, new String[] { err, this.name, this.correlationId } );
    }
}
