package io.deepstream.rpc;


import com.google.gson.JsonObject;
import io.deepstream.ConnectionMock;
import io.deepstream.DeepstreamClient;
import io.deepstream.constants.Actions;
import io.deepstream.constants.Topic;
import io.deepstream.message.Message;
import io.deepstream.util.Util;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.net.URISyntaxException;
import java.util.Map;
import java.util.Properties;

import static org.mockito.Mockito.*;

@RunWith( JUnit4.class )
public class RpcTest {

    DeepstreamClient deepstreamClientMock;
    ConnectionMock connectionMock;
    RpcHandler rpcHandler;
    RpcResponseCallback callbackMock;
    RpcCallback addTwoCallback = new RpcCallback() {
        @Override
        public void Call(Object data, RpcResponse response) {
            Map m = (Map) data;
            double numA = (double) m.get("numA");
            double numB = (double) m.get("numB");
            response.send(numA + numB);
        }
    };
    int rpcCalls = 0;

    @Before
    public void setUp() throws URISyntaxException {
        this.deepstreamClientMock = mock(DeepstreamClient.class);
        when(this.deepstreamClientMock.getUid()).thenReturn("1");

        this.connectionMock = new ConnectionMock();
        this.callbackMock = mock(RpcResponseCallback.class);

        Properties options = new Properties();
        options.put("subscriptionTimeout", 2000);
        options.put("rpcAckTimeout", 6000);
        options.put("rpcResponseTimeout", 10000);
        this.rpcHandler = new RpcHandler(options, connectionMock, deepstreamClientMock);

    }

    @After
    public void tearDown() {

    }

    @Test
    public void makesSuccessfulRpcFor_addTwo() {
        JsonObject data = new JsonObject();
        data.addProperty("numA", 3);
        data.addProperty("numB", 8);
        rpcHandler.make("addTwo", data, this.callbackMock);

        Assert.assertEquals(Util.convertChars("P|REQ|addTwo|1|O{\"numA\":3,\"numB\":8}+"), connectionMock.lastSentMessage);

        rpcHandler.handle(new Message(
                "raw",
                Topic.RPC,
                Actions.RESPONSE,
                new String[]{ "addTwo", "1", "N11" }
        ));
        verify(callbackMock, times(1)).onData( (float) 11.0 );
    }

    @Test
    public void makesRpcFor_addTwoButReceivesError() {
        JsonObject data = new JsonObject();
        data.addProperty("numA", 3);
        data.addProperty("numB", 8);
        rpcHandler.make("addTwo", data, this.callbackMock);

        Assert.assertEquals(Util.convertChars("P|REQ|addTwo|1|O{\"numA\":3,\"numB\":8}+"), connectionMock.lastSentMessage);

        rpcHandler.handle(new Message(
                "raw",
                Topic.RPC,
                Actions.ERROR,
                new String[]{ "NO_PROVIDER", "addTwo", "1" }
        ));
        verify(callbackMock, times(1)).onError( "NO_PROVIDER" );
    }

}