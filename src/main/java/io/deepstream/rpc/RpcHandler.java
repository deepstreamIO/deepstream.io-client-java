package io.deepstream.rpc;

import io.deepstream.DeepstreamClient;
import io.deepstream.DeepstreamException;
import io.deepstream.IConnection;
import io.deepstream.constants.Actions;
import io.deepstream.constants.Topic;
import io.deepstream.utils.AckTimeoutRegistry;
import io.deepstream.utils.ResubscribeNotifier;

import java.util.HashMap;
import java.util.Map;

public class RpcHandler {

    private Map options;
    private IConnection connection;
    private DeepstreamClient client;
    private Map<String, RpcCallback> providers;
    private AckTimeoutRegistry ackTimeoutRegistry;
    private ResubscribeNotifier resubscribeNotifier;

    public RpcHandler(Map options, IConnection connection, DeepstreamClient client ) {
        this.options = options;
        this.connection = connection;
        this.client = client;
        this.providers = new HashMap<>();

        long timeoutDuration = (long) this.options.get( "subscriptionTimeout" );
        this.ackTimeoutRegistry = new AckTimeoutRegistry( this.client, Topic.RPC, timeoutDuration );
    }

    public void provide( String name, RpcCallback callback ) {
        if( this.providers.containsKey( name ) ) {
            throw new DeepstreamException( "RPC " + name + " already registered" );
        }

        this.ackTimeoutRegistry.add( name, Actions.SUBSCRIBE );
        this.providers.put( name, callback );
        this.connection.sendMsg( Topic.RPC, Actions.SUBSCRIBE, name );
    }

    public void unprovide( String name ) {
        if( this.providers.containsKey( name ) ) {
            this.providers.remove( name );
            this.ackTimeoutRegistry.add( name, Actions.UNSUBSCRIBE );
            this.connection.sendMsg( Topic.RPC, Actions.UNSUBSCRIBE, name );
        }
    }
}
