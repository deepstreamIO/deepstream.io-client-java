package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CountDownLatch;

/**
 * The entry point for rpcs, both requesting them via {@link RpcHandler#make(String, Object)} and
 * providing them via {@link RpcHandler#provide(String, RpcRequestedListener)}
 */
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
    @ObjectiveCName("init:connection:client:")
    RpcHandler(DeepstreamConfig deepstreamConfig, final IConnection connection, DeepstreamClientAbstract client) {
        this.deepstreamConfig = deepstreamConfig;
        this.connection = connection;
        this.client = client;
        this.providers = new HashMap<>();
        this.rpcs = new HashMap<>();
        this.ackTimeoutRegistry = client.getAckTimeoutRegistry();
        new UtilResubscribeNotifier(this.client, new UtilResubscribeNotifier.UtilResubscribeListener() {
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
     * {@link RpcHandler#make(String, Object)} the request will be routed to the supplied listener.
     * <br/>
     * Only one listener can be registered for a RPC at a time.
     * <br/>
     * Please note: Deepstream tries to deliver data in its original format. Data passed to
     * {@link RpcHandler#make(String, Object)} as a String will arrive as a String, numbers or
     *  implicitly JSON serialized objects will arrive in their respective format as well.
     *
     * @param rpcName The rpcName of the RPC to provide
     * @param rpcRequestedListener The listener to invoke when requests are received
     */
    @ObjectiveCName("provide:rpcRequestedListener:")
    public void provide( String rpcName, RpcRequestedListener rpcRequestedListener ) {
        if( this.providers.containsKey( rpcName ) ) {
            throw new DeepstreamException( "RPC " + rpcName + " already registered" );
        }

        synchronized (this) {
            this.providers.put(rpcName, rpcRequestedListener);
            this.sendRPCSubscribe(rpcName);
        }
    }

    /**
     * Unregister a {@link RpcRequestedListener} registered via Rpc{@link #provide(String, RpcRequestedListener)}
     * @param rpcName The rpcName of the RPC to stop providing
     */
    @ObjectiveCName("unprovide:")
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
     * @return Find out if the rpc succeeded via {@link RpcResult#success()} and associated data via {@link RpcResult#getData()}
     */
    @ObjectiveCName("make:data:")
    public RpcResult make(String rpcName, Object data) {
        final RpcResult[] rpcResponse = new RpcResult[1];
        final CountDownLatch responseLatch = new CountDownLatch(1);


        synchronized (this) {
            String uid = this.client.getUid();
            this.rpcs.put(uid, new Rpc(this.deepstreamConfig, this.client, rpcName, uid, new RpcResponseCallback() {
                @Override
                public void onRpcSuccess(String rpcName, Object data) {
                    rpcResponse[0] = new RpcResult(true, data);
                    responseLatch.countDown();
                }

                @Override
                public void onRpcError(String rpcName, Object error) {
                    rpcResponse[0] = new RpcResult(false, error);
                    responseLatch.countDown();
                }
            }));

            String typedData = MessageBuilder.typed(data);
            this.connection.sendMsg(Topic.RPC, Actions.REQUEST, new String[]{rpcName, uid, typedData});
        }

        try {
            responseLatch.await();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        return rpcResponse[0];
    }

    /**
     * Main interface. Handles incoming messages
     * from the message distributor
     * @param message The message recieved from the server
     */
    @ObjectiveCName("handle:")
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
        if( message.action == Actions.ERROR || message.action == Actions.ACK ) {
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
    @ObjectiveCName("getRpc:raw:")
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
    @ObjectiveCName("respondToRpc:")
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
            response = new RpcResponse(this.connection, rpcName, correlationId);
            callback.onRPCRequested(rpcName, data, response);
        } else {
            this.connection.sendMsg( Topic.RPC, Actions.REJECTION, new String[] { rpcName, correlationId } );
        }
    }

    /**
     * Send an unsubscribe or subscribe event if the connection is open
     */
    @ObjectiveCName("sendRPCSubscribe:")
    private void sendRPCSubscribe(String rpcName) {
        if( this.client.getConnectionState() == ConnectionState.OPEN ) {
            this.ackTimeoutRegistry.add(Topic.RPC, Actions.SUBSCRIBE, rpcName, deepstreamConfig.getSubscriptionTimeout());
            this.connection.sendMsg(Topic.RPC, Actions.SUBSCRIBE, new String[]{rpcName});
        }
    }

    /**
     * The callback for an rpc that has been requested by the client
     */
    interface RpcResponseCallback {
        /**
         * Called when the rpc was completed successfully by another client that has provided the rpc via
         * {@link RpcHandler#provide(String, RpcRequestedListener)}
         *
         * @param rpcName The rpc name
         * @param data    The result data from the rpc
         */
        @ObjectiveCName("onRpcSuccess:data:")
        void onRpcSuccess(String rpcName, Object data);

        /**
         * Called when the rpc was completed unsuccessfully by another client that has provided the rpc via
         * {@link RpcHandler#provide(String, RpcRequestedListener)}
         *
         * @param rpcName The rpc name
         */
        @ObjectiveCName("onRpcError:error:")
        void onRpcError(String rpcName, Object error);
    }
}
