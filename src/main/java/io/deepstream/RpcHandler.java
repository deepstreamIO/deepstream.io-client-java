package io.deepstream;

import com.google.gson.JsonObject;
import io.deepstream.constants.Actions;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.util.HashMap;
import java.util.Map;

class RpcHandler implements UtilResubscribeCallback {

    private int timeoutDuration;
    private Map options;
    private IConnection connection;
    private IDeepstreamClient client;
    private Map<String, RpcRequested> providers;
    private UtilAckTimeoutRegistry ackTimeoutRegistry;
    private UtilResubscribeNotifier resubscribeNotifier;
    private Map<String, Rpc> rpcs;

    public RpcHandler( Map options, IConnection connection, IDeepstreamClient client ) {
        this.options = options;
        this.connection = connection;
        this.client = client;
        this.providers = new HashMap<>();
        this.rpcs = new HashMap<>();
        this.ackTimeoutRegistry = UtilAckTimeoutRegistry.getAckTimeoutRegistry( this.client );
        this.resubscribeNotifier = new UtilResubscribeNotifier( this.client, this );

        this.timeoutDuration = Integer.parseInt( (String) this.options.get( "subscriptionTimeout" ) );
    }

    public void provide( String name, RpcRequested callback ) {
        if( this.providers.containsKey( name ) ) {
            throw new DeepstreamException( "RPC " + name + " already registered" );
        }

        this.ackTimeoutRegistry.add( Topic.RPC, Actions.SUBSCRIBE, name, this.timeoutDuration );
        this.providers.put( name, callback );
        this.connection.sendMsg( Topic.RPC, Actions.SUBSCRIBE, new String[] { name } );
    }

    public void unprovide( String name ) {
        if( this.providers.containsKey( name ) ) {
            this.providers.remove( name );
            this.ackTimeoutRegistry.add( Topic.RPC, Actions.UNSUBSCRIBE, name, this.timeoutDuration);
            this.connection.sendMsg( Topic.RPC, Actions.UNSUBSCRIBE, new String[] { name } );
        }
    }

    public void make(String name, JsonObject data, RpcResponseCallback callback ) {
        make( name, MessageBuilder.typed( data ), callback );
    }

    public void make(String name, String data, RpcResponseCallback callback ) {
        String uid = this.client.getUid();
        this.rpcs.put( uid, new Rpc( this.options, this.client, callback ) );
        this.connection.sendMsg( Topic.RPC, Actions.REQUEST, new String[] { name, uid, data } );
    }

    public void handle( Message message ) {
        String rpcName, correlationId;
        Rpc rpc;

        // RPC Requests
        if( message.action == Actions.REQUEST ) {
            this.respondToRpc( message );
            return;
        }
        // RPC subscription Acks
        if( message.action == Actions.ACK &&
                ( message.data[ 0 ].equals( Actions.SUBSCRIBE.toString() ) || message.data[ 0 ].equals( Actions.UNSUBSCRIBE.toString() ) ) ) {
            this.ackTimeoutRegistry.clear( message );
            return;
        }

        /*
         * Error messages always have the error as first parameter. So the
         * order is different to ack and response messages
         */
        if( message.action == Actions.ERROR ) {
            rpcName = message.data[ 1 ];
            correlationId = message.data[ 2 ];
        } else {
            rpcName = message.data[ 0 ];
            correlationId = message.data[ 1 ];
        }

        /*
        * Retrieve the rpc object
        */
        rpc = this.getRpc( correlationId, rpcName, message.raw );
        if( rpc == null ) {
            return;
        }

        // RPC Responses
        if( message.action == Actions.ACK ) {
            rpc.ack();
        }
        else if( message.action == Actions.RESPONSE ) {
            rpc.respond( message.data[ 2 ] );
            this.rpcs.remove( correlationId );
        }
        else if( message.action == Actions.ERROR ) {
            rpc.error( message.data[ 0 ] );
            this.rpcs.remove( correlationId );
        }
    }

    private Rpc getRpc(String correlationId, String rpcName, String raw) {
        Rpc rpc = this.rpcs.get( correlationId );

        if( rpc == null ) {
            this.client.onError( Topic.RPC, Event.UNSOLICITED_MESSAGE, raw );
        }

        return rpc;
    }

    private void respondToRpc( Message message ) {
        String name = message.data[ 0 ],
                correlationId = message.data[ 1 ];
        RpcResponse response;
        Object data = null;

        if( message.data[ 2 ] != null ) {
            data = MessageParser.convertTyped( message.data[ 2 ], this.client );
        }

        RpcRequested callback = this.providers.get( name );
        if( callback != null ) {
            response = new RpcResponse( this.connection, name, correlationId );
            callback.Call( data, response );
        } else {
            this.connection.sendMsg( Topic.RPC, Actions.REJECTION, new String[] { name, correlationId } );
        }
    }

    public void resubscribe() {
        for ( String name : providers.keySet() ) {
            connection.sendMsg( Topic.RPC, Actions.SUBSCRIBE, new String[] { name } );
        }
    }
}
