package io.deepstream;


import io.deepstream.constants.Actions;
import io.deepstream.constants.Topic;

public class RpcResponse {

    private IConnection connection;
    private String name;
    private String correlationId;
    private boolean isAcknowledged;
    private boolean isComplete;

    /**
     * This object provides a number of methods that allow a rpc provider
     * to respond to a request
     * @param connection the clients connection object
     * @param name the name of the rpc
     * @param correlationId the correlationId for the RPC
     */
    RpcResponse( IConnection connection, String name, String correlationId ) {
        this.connection = connection;
        this.name = name;
        this.correlationId = correlationId;
        this.isAcknowledged = false;
        this.isComplete = false;
        this.ack();
    }

    /**
     * Acknowledges the receipt of the request. This
     * will happen implicitly unless the request callback
     * explicitly sets autoAck to false
     */
    public void ack() {
        if(!this.isAcknowledged) {
            this.connection.sendMsg( Topic.RPC, Actions.ACK, new String[] { this.name, this.correlationId } );
            this.isAcknowledged = true;
        }
    }

    /**
     * Reject the request. This might be necessary if the client
     * is already processing a large number of requests. If deepstream
     * receives a rejection message it will try to route the request to
     * another provider - or return a NO_RPC_PROVIDER error if there are no
     * providers left
     */
    public void reject() {
        this.isComplete = true;
        this.isAcknowledged = true;
        this.connection.sendMsg( Topic.RPC, Actions.REJECTION, new String[] { this.name, this.correlationId } );
    }

    /**
     * Completes the request by sending the response data
     * to the server. If data is an array or object it will
     * automatically be serialised.<br/>
     * If autoAck is disabled and the response is sent before
     * the ack message the request will still be completed and the
     * ack message ignored
     *
     * @param data the data send by the provider. Has to be JsonSerializable
     */
    public void send( Object data ) {
        if(this.isComplete) {
            throw new DeepstreamException( "Rpc " + this.name + " already completed" );
        }
        String typedData = MessageBuilder.typed( data );
        this.connection.sendMsg( Topic.RPC, Actions.RESPONSE, new String[] {
                this.name, this.correlationId, typedData
        } );
        this.isComplete = true;
    }

    /**
     * Notifies the server that an error has occured while trying to process the request.
     * This will complete the rpc.
     *
     * @param errorMsg the message used to describe the error that occured
     */
    public void error( String errorMsg ) {
        this.isComplete = true;
        this.isAcknowledged = true;
        this.connection.sendMsg( Topic.RPC, Actions.ERROR, new String[] { errorMsg, this.name, this.correlationId } );
    }
}
