package io.deepstream;

import com.google.gson.JsonObject;
import io.deepstream.constants.Actions;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;
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
public class RpcHandlerTest {

    DeepstreamClientMock deepstreamClientMock;
    ConnectionMock connectionMock;
    RpcHandler rpcHandler;
    RpcResponseCallback callbackMock;
    int rpcCalls = 0;
    RpcRequested addTwoCallback = new RpcRequested() {
        @Override
        public void Call(Object data, RpcResponse response) {
            rpcCalls++;
            Map m = (Map) data;
            double numA = (double) m.get( "numA" );
            double numB = (double) m.get( "numB" );
            response.send( numA + numB );
        }
    };
    ErrorCallback errorCallbackMock;


    @Before
    public void setUp() throws URISyntaxException {
        this.callbackMock = mock( RpcResponseCallback.class );
        this.connectionMock = new ConnectionMock();
        this.errorCallbackMock = mock( ErrorCallback.class );
        this.deepstreamClientMock = new DeepstreamClientMock( this.errorCallbackMock );
        this.deepstreamClientMock.setConnectionState( ConnectionState.OPEN );

        Properties options = new Properties();
        options.put( "subscriptionTimeout", "10" );
        options.put( "rpcAckTimeout", "10" );
        options.put( "rpcResponseTimeout", "30" );
        this.rpcHandler = new RpcHandler( options, connectionMock, deepstreamClientMock );
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
        verify( errorCallbackMock, times(1) ).onError( Topic.RPC, Event.ACK_TIMEOUT, "No ACK message received in time for SUBSCRIBE addTwo" );
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
    public void makesSuccessfulRpcFor_addTwo() {
        JsonObject data = new JsonObject();
        data.addProperty("numA", 3);
        data.addProperty("numB", 8);
        rpcHandler.make("addTwo", data, this.callbackMock);

        Assert.assertEquals(TestUtil.replaceSeperators("P|REQ|addTwo|1|O{\"numA\":3,\"numB\":8}+"), connectionMock.lastSentMessage);

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

        Assert.assertEquals(TestUtil.replaceSeperators("P|REQ|addTwo|1|O{\"numA\":3,\"numB\":8}+"), connectionMock.lastSentMessage);

        rpcHandler.handle(new Message(
                "raw",
                Topic.RPC,
                Actions.ERROR,
                new String[]{ "NO_PROVIDER", "addTwo", "1" }
        ));
        verify(callbackMock, times(1)).onError( "NO_PROVIDER" );
    }

    @Test
    public void makesRpcFor_addTwoButDoesntReceiveAck() throws InterruptedException {
        JsonObject data = new JsonObject();
        data.addProperty("numA", 3);
        data.addProperty("numB", 8);
        rpcHandler.make("addTwo", data, this.callbackMock);

        Assert.assertEquals( TestUtil.replaceSeperators("P|REQ|addTwo|1|O{\"numA\":3,\"numB\":8}+"), connectionMock.lastSentMessage);

        Thread.sleep(50);
        verify(this.errorCallbackMock, times(1)).onError( Topic.RPC, Event.ACK_TIMEOUT, "No ACK message received in time for REQUEST 1" );
    }

    @Test
    public void makesRpcFor_addTwoButDoesntReceiveResponse() throws InterruptedException {
        JsonObject data = new JsonObject();
        data.addProperty("numA", 3);
        data.addProperty("numB", 8);
        rpcHandler.make("addTwo", data, this.callbackMock);

        Assert.assertEquals( TestUtil.replaceSeperators("P|REQ|addTwo|1|O{\"numA\":3,\"numB\":8}+"), connectionMock.lastSentMessage);

        Thread.sleep(40);
        verify(callbackMock, times(1)).onError( Event.RESPONSE_TIMEOUT.toString() );
    }
}
