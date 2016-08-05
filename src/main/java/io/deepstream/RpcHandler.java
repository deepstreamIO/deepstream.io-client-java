package io.deepstream;

import io.deepstream.constants.Actions;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.util.HashMap;
import java.util.Map;

public class RpcHandler {
    private final DeepstreamConfig deepstreamConfig;
    private final IConnection connection;
    private final DeepstreamClientAbstract client;
    private final Map<String, RpcRequestedListener> providers;
    private final UtilAckTimeoutRegistry ackTimeoutRegistry;
    private final Map<String, Rpc> rpcs;

    /**
     * The main class for remote procedure calls
     *
     * Provides the rpc interface and handles incoming messages
     * on the rpc topic
     *
     * @param deepstreamConfig The deepstreamConfig the client was created with
     * @param connection The connection to deepstream
     * @param client The deepstream client
     */
    RpcHandler(DeepstreamConfig deepstreamConfig, final IConnection connection, DeepstreamClientAbstract client) {
        this.deepstreamConfig = deepstreamConfig;
        this.connection = connection;
        this.client = client;
        this.providers = new HashMap<>();
        this.rpcs = new HashMap<>();
        this.ackTimeoutRegistry = client.getAckTimeoutRegistry();
        new UtilResubscribeNotifier(this.client, new UtilResubscribeCallback() {
            @Override
            public void resubscribe() {
                for (String rpcName : providers.keySet()) {
                    sendRPCSubscribe(rpcName);
                }
            }
        });
    }

    /**
     * Registers a {@link RpcRequestedListener} as a RPC provider. If another connected client calls
     * {@link RpcHandler#make(String, Object, RpcResponseCallback)} the request will be routed to the supplied listener.
     * <br/>
     * Only one listener can be registered for a RPC at a time.
     * <br/>
     * Please note: Deepstream tries to deliver data in its original format. Data passed to
     * {@link RpcHandler#make(String, Object, RpcResponseCallback)} as a String will arrive as a String, numbers or
     *  implicitly JSON serialized objects will arrive in their respective format as well.
     *
     * @param rpcName The rpcName of the RPC to provide
     * @param rpcRequestedListener The listener to invoke when requests are recieved
     */
    public void provide( String rpcName, RpcRequestedListener rpcRequestedListener ) {
        if( this.providers.containsKey( rpcName ) ) {
            throw new DeepstreamException( "RPC " + rpcName + " already registered" );
        }

        this.providers.put( rpcName, rpcRequestedListener );
        this.sendRPCSubscribe( rpcName );
    }

    /**
     * Unregister a {@link RpcRequestedListener} registered via Rpc{@link #provide(String, RpcRequestedListener)}
     * @param rpcName The rpcName of the RPC to stop providing
     */
    public void unprovide( String rpcName ) {
        if( this.providers.containsKey( rpcName ) ) {
            this.providers.remove( rpcName );

            this.ackTimeoutRegistry.add(Topic.RPC, Actions.UNSUBSCRIBE, rpcName, deepstreamConfig.getSubscriptionTimeout());
            this.connection.sendMsg(Topic.RPC, Actions.UNSUBSCRIBE, new String[]{rpcName});
        }
    }

    /**
     * Create a remote procedure call. This requires a rpc name for routing, a JSON serializable object for any associated
     * arguments and a callback to notify you with the rpc result or potential error.
     * @param rpcName The name of the rpc
     * @param data Serializable data that will be passed to the provider
     * @param callback  Will be invoked with {@link RpcResponseCallback#onRpcSuccess(String, Object)} or {@link RpcResponseCallback#onRpcError(String, Object)}
     */
    public void make(String rpcName, Object data, RpcResponseCallback callback ) {
        String uid = this.client.getUid();
        this.rpcs.put(uid, new Rpc(this.deepstreamConfig, this.client, rpcName, uid, callback));

        String typedData = MessageBuilder.typed( data );
        this.connection.sendMsg( Topic.RPC, Actions.REQUEST, new String[] { rpcName, uid, typedData } );
    }

    /**
     * Main interface. Handles incoming messages
     * from the message distributor
     * @param message The message recieved from the server
     */
    void handle( Message message ) {
        String rpcName;
        String correlationId;
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
        rpc = this.getRpc( correlationId, message.raw );
        if( rpc == null ) {
            return;
        }

        // RPC Responses
        if( message.action == Actions.ACK ) {
            rpc.ack();
        }
        else if( message.action == Actions.RESPONSE ) {
            rpc.respond( rpcName, message.data[ 2 ] );
            this.rpcs.remove( correlationId );
        }
        else if( message.action == Actions.ERROR ) {
            rpc.error( rpcName, message.data[ 0 ] );
            this.rpcs.remove( correlationId );
        }
    }

    /**
     * Retrieves a RPC instance for a correlationId or throws an error
     * if it can't be found (which should never happen)
     */
    private Rpc getRpc(String correlationId, String raw) {
        Rpc rpc = this.rpcs.get( correlationId );

        if( rpc == null ) {
            this.client.onError( Topic.RPC, Event.UNSOLICITED_MESSAGE, raw );
        }

        return rpc;
    }

    /**
     * Handles incoming rpc REQUEST messages. Instantiates a new response object
     * and invokes the provider callback or rejects the request if no rpc provider
     * is present (which shouldn't really happen, but might be the result of a race condition
     * if this client sends a unprovide message whilst an incoming request is already in flight)
     */
    private void respondToRpc( Message message ) {
        String rpcName = message.data[ 0 ];
        String correlationId = message.data[ 1 ];
        RpcResponse response;
        Object data = null;

        if( message.data[ 2 ] != null ) {
            data = MessageParser.convertTyped( message.data[ 2 ], this.client );
        }

        RpcRequestedListener callback = this.providers.get( rpcName );
        if( callback != null ) {
            response = new RpcResponse( this.connection, rpcName, correlationId );
            callback.onRPCRequested(rpcName, data, response);
        } else {
            this.connection.sendMsg( Topic.RPC, Actions.REJECTION, new String[] { rpcName, correlationId } );
        }
    }

    /**
     * Send an unsubscribe or subscribe event if the connection is open
     */
    private void sendRPCSubscribe(String rpcName) {
        if( this.client.getConnectionState() == ConnectionState.OPEN ) {
            this.ackTimeoutRegistry.add(Topic.RPC, Actions.SUBSCRIBE, rpcName, deepstreamConfig.getSubscriptionTimeout());
            this.connection.sendMsg(Topic.RPC, Actions.SUBSCRIBE, new String[]{rpcName});
        }
    }
}
