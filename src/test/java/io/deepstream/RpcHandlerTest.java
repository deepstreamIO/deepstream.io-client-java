package io.deepstream;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.net.URISyntaxException;
import java.util.Properties;

import static org.mockito.Mockito.*;

@RunWith( JUnit4.class )
public class RpcHandlerTest {

    DeepstreamClientMock deepstreamClientMock;
    ConnectionMock connectionMock;
    RpcHandler rpcHandler;
    int rpcCalls = 0;
    RpcRequestedListener addTwoCallback = new RpcRequestedListener() {
        @Override
        public void onRPCRequested(String rpcName, Object data, RpcResponse response) {
            rpcCalls++;
            double numA = ((JsonElement) data).getAsJsonObject().get("numA").getAsDouble();
            double numB = ((JsonElement) data).getAsJsonObject().get("numB").getAsDouble();
            response.send( numA + numB );
        }
    };
    DeepstreamRuntimeErrorHandler errorCallbackMock;


    @Before
    public void setUp() throws URISyntaxException, InvalidDeepstreamConfig {
        this.connectionMock = new ConnectionMock();
        this.connectionMock.state = ConnectionState.OPEN;
        this.errorCallbackMock = mock( DeepstreamRuntimeErrorHandler.class );
        this.deepstreamClientMock = new DeepstreamClientMock();
        this.deepstreamClientMock.setRuntimeErrorHandler( errorCallbackMock );
        this.deepstreamClientMock.setConnectionState( ConnectionState.OPEN );

        Properties options = new Properties();
        options.put( "subscriptionTimeout", "10" );
        options.put( "rpcAckTimeout", "10" );
        options.put( "rpcResponseTimeout", "30" );
        this.rpcHandler = new RpcHandler( new DeepstreamConfig( options ), connectionMock, deepstreamClientMock );
    }

    @After
    public void tearDown() {

    }

    @Test
    public void registersAProvider() {
        Assert.assertNull( connectionMock.lastSentMessage );
        rpcHandler.provide( "addTwo", addTwoCallback );
        Assert.assertEquals( TestUtil.replaceSeperators("P|S|addTwo+"), connectionMock.lastSentMessage );
        Assert.assertEquals( rpcCalls, 0 );
    }

    @Test
    public void errorsIfNoAckReceivedForProvide() throws InterruptedException {
        rpcHandler.provide( "addTwo", addTwoCallback );
        Thread.sleep(50);
        verify( errorCallbackMock, times(1) ).onException( Topic.RPC, Event.ACK_TIMEOUT, "No ACK message received in time for SUBSCRIBE addTwo" );
    }

    @Test
    public void repliesToRpcRequest() {
        rpcHandler.provide( "addTwo", addTwoCallback );

        rpcHandler.handle( new Message(
                "raw",
                Topic.RPC,
                Actions.REQUEST,
                new String[] { "addTwo", "123", "O{\"numA\":7,\"numB\":3}" }
        ));
        Assert.assertEquals( TestUtil.replaceSeperators( "P|RES|addTwo|123|N10.0+" ), connectionMock.lastSentMessage );
    }

    @Test
    public void sendsRejectionIfNoProviderExists() {
        rpcHandler.handle(
                new Message(
                        "raw",
                        Topic.RPC,
                        Actions.REQUEST,
                        new String[] { "doesNotExist", "123", "O{\"numA\":7,\"numB\":3}" })
        );
        Assert.assertEquals( TestUtil.replaceSeperators( "P|REJ|doesNotExist|123+" ), connectionMock.lastSentMessage );
    }

    @Test
    public void deregistersAProvider() {
        rpcHandler.provide( "addTwo", addTwoCallback );
        rpcHandler.unprovide( "addTwo" );
        Assert.assertEquals( TestUtil.replaceSeperators("P|US|addTwo+"), connectionMock.lastSentMessage );
    }

    @Test
    public void doesntCallDeregisteredProvider() {
        rpcHandler.provide( "addTwo", addTwoCallback );
        rpcHandler.unprovide( "addTwo" );

        rpcHandler.handle(
                new Message(
                        "raw",
                        Topic.RPC,
                        Actions.REQUEST,
                        new String[] { "doesNotExist", "123", "O{\"numA\":7,\"numB\":3}" })
        );
        Assert.assertEquals( TestUtil.replaceSeperators( "P|REJ|doesNotExist|123+" ), connectionMock.lastSentMessage );
    }

    @Test
    public void makesSuccessfulRpcFor_addTwo() throws InterruptedException {
        final JsonObject data = new JsonObject();
        data.addProperty("numA", 3);
        data.addProperty("numB", 8);

        final RpcResult[] rpcResponse = new RpcResult[1];
        new Thread(new Runnable() {
            @Override
            public void run() {
                rpcResponse[0] = rpcHandler.make("addTwo", data);
            }
        }).start();

        Thread.sleep( 20 );
        rpcHandler.handle(new Message(
                "raw",
                Topic.RPC,
                Actions.RESPONSE,
                new String[]{ "addTwo", "1", "N11" }
        ));
        Thread.sleep( 20 );

        Assert.assertEquals(TestUtil.replaceSeperators("P|REQ|addTwo|1|O{\"numA\":3,\"numB\":8}+"), connectionMock.lastSentMessage);
        Assert.assertTrue( rpcResponse[0].success() );
        Assert.assertEquals( rpcResponse[0].getData(), (float) 11.0 );
    }

    @Test
    public void makesRpcFor_addTwoButReceivesError() throws InterruptedException {
        final JsonObject data = new JsonObject();
        data.addProperty("numA", 3);
        data.addProperty("numB", 8);

        final RpcResult[] rpcResponse = new RpcResult[1];
        new Thread(new Runnable() {
            @Override
            public void run() {
                rpcResponse[0] = rpcHandler.make("addTwo", data);
            }
        }).start();

        Thread.sleep( 20 );
        Assert.assertEquals(TestUtil.replaceSeperators("P|REQ|addTwo|1|O{\"numA\":3,\"numB\":8}+"), connectionMock.lastSentMessage);

        rpcHandler.handle(new Message(
                "raw",
                Topic.RPC,
                Actions.ERROR,
                new String[]{ "NO_PROVIDER", "addTwo", "1" }
        ));

        Thread.sleep( 20 );

        Assert.assertEquals(TestUtil.replaceSeperators("P|REQ|addTwo|1|O{\"numA\":3,\"numB\":8}+"), connectionMock.lastSentMessage);
        Assert.assertFalse( rpcResponse[0].success() );
        Assert.assertEquals( rpcResponse[0].getData(), "NO_PROVIDER" );
    }

    @Test
    public void makesRpcFor_addTwoButDoesntReceiveAck() throws InterruptedException {
        final JsonObject data = new JsonObject();
        data.addProperty("numA", 3);
        data.addProperty("numB", 8);

        final RpcResult[] rpcResponse = new RpcResult[1];
        new Thread(new Runnable() {
            @Override
            public void run() {
                rpcResponse[0] = rpcHandler.make("addTwo", data);
            }
        }).start();

        Thread.sleep(200);
        Assert.assertEquals( TestUtil.replaceSeperators("P|REQ|addTwo|1|O{\"numA\":3,\"numB\":8}+"), connectionMock.lastSentMessage);
        Thread.sleep(250);

        verify(this.errorCallbackMock, times(1)).onException( Topic.RPC, Event.ACK_TIMEOUT, "No ACK message received in time for REQUEST 1" );
    }

    @Test
    public void makesRpcFor_addTwoButDoesntReceiveResponse() throws InterruptedException {
        final JsonObject data = new JsonObject();
        data.addProperty("numA", 3);
        data.addProperty("numB", 8);

        final RpcResult[] rpcResponse = new RpcResult[1];
        new Thread(new Runnable() {
            @Override
            public void run() {
                rpcResponse[0] = rpcHandler.make("addTwo", data);
            }
        }).start();

        Thread.sleep( 20 );
        Assert.assertEquals( TestUtil.replaceSeperators("P|REQ|addTwo|1|O{\"numA\":3,\"numB\":8}+"), connectionMock.lastSentMessage);

        Thread.sleep(100);
        Assert.assertFalse( rpcResponse[0].success() );
        Assert.assertEquals( rpcResponse[0].getData(),Event.RESPONSE_TIMEOUT.toString() );
    }
}
