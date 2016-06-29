package io.deepstream.rpc;

import io.deepstream.DeepstreamClient;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;
import io.deepstream.message.MessageParser;

import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;


public class Rpc {

    Map properties;
    DeepstreamClient client;
    RpcResponseCallback callback;
    TimerTask ackTimeout;
    TimerTask responseTimeout;

    public Rpc(Map properties, DeepstreamClient client, RpcResponseCallback callback ) {
        this.properties = properties;
        this.client = client;
        this.callback = callback;
        this.setTimeouts();
    }

    public void ack() {
        this.ackTimeout.cancel();
    }

    public void respond( String data ) {
        Object convertedData = MessageParser.convertTyped( data );
        this.callback.onData( convertedData );
        this.clearTimeouts();
    }

    public void error(String err ) {
        this.callback.onError( err );
        this.clearTimeouts();
    }

    private void clearTimeouts() {
        this.ackTimeout.cancel();
        this.responseTimeout.cancel();
    }

    private void setTimeouts() {
        final Rpc self = this;
        this.ackTimeout = new TimerTask() {
            public void run() {
                client.onError( Topic.RPC, Event.ACK_TIMEOUT, null );
            }
        };
        this.responseTimeout = new TimerTask() {
            public void run() {client.onError( Topic.RPC, Event.RESPONSE_TIMEOUT, null );
            }
        };

        Timer timer = new Timer();
        int ackTimeoutTime = Integer.parseInt( (String) properties.get( "rpcAckTimeout" ) );
        int responseTimeoutTime = Integer.parseInt( (String) properties.get( "rpcResponseTimeout" ) );
        timer.schedule( this.ackTimeout, ackTimeoutTime );
        timer.schedule( this.responseTimeout, responseTimeoutTime );
    }
}
