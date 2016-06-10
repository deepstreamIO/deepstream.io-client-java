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

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RunWith( JUnit4.class )
public class RpcHandlerTest {

    DeepstreamClient deepstreamClientMock;
    ConnectionMock connectionMock;
    RpcHandler rpcHandler;
    RpcCallback addTwoCallback = new RpcCallback() {
        @Override
        public void Call(Object data, RpcResponse response) {
            Map m = (Map) data;
            double numA = (double) m.get( "numA" );
            double numB = (double) m.get( "numB" );
            response.send( numA + numB );
        }
    };
    int rpcCalls = 0;

    @Before
    public void setUp() throws URISyntaxException {
        this.deepstreamClientMock = mock( DeepstreamClient.class );
        when( this.deepstreamClientMock.getUid() ).thenReturn( "1" );

        this.connectionMock = new ConnectionMock();
        Properties options = new Properties();
        options.put( "subscriptionTimeout", 2000 );
        options.put( "rpcAckTimeout", 6000 );
        options.put( "rpcResponseTimeout", 10000 );
        this.rpcHandler = new RpcHandler( options, connectionMock, deepstreamClientMock );

    }

    @After
    public void tearDown() {

    }

    @Test
    public void registersAProvider() {
        Assert.assertNull( connectionMock.lastSentMessage );
        rpcHandler.provide( "addTwo", addTwoCallback );
        Assert.assertEquals( Util.convertChars("P|S|addTwo+"), connectionMock.lastSentMessage );
        Assert.assertEquals( rpcCalls, 0 );
    }

    @Test
    public void deregistersAProvider() {
        rpcHandler.provide( "addTwo", addTwoCallback );
        rpcHandler.unprovide( "addTwo" );
        Assert.assertEquals( Util.convertChars("P|US|addTwo+"), connectionMock.lastSentMessage );
    }

    @Test
    public void makesAnRpcForAddTwo() {
        JsonObject params = new JsonObject();
        params.addProperty( "numA", 3 );
        params.addProperty( "numB", 8 );
        rpcHandler.make("addTwo", params, new RpcResponseCallback() {
            @Override
            public void onData(Object data) {

            }

            @Override
            public void onError(String err) {

            }
        });
        Assert.assertEquals( Util.convertChars("P|REQ|addTwo|1|O{\"numA\":3,\"numB\":8}+"), connectionMock.lastSentMessage );
    }

    @Test
    public void repliesToSyncRpcRequest() {
        rpcHandler.provide( "addTwo", addTwoCallback );

        rpcHandler.handle( new Message(
                "raw",
                Topic.RPC,
                Actions.REQUEST,
                new String[] { "addTwo", "123", "O{\"numA\":7,\"numB\":3}" }
        ));
        Assert.assertEquals( Util.convertChars( "P|RES|addTwo|123|N10.0+" ), connectionMock.lastSentMessage );
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
        Assert.assertEquals( Util.convertChars( "P|REJ|doesNotExist|123+" ), connectionMock.lastSentMessage );
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
        Assert.assertEquals( Util.convertChars( "P|REJ|doesNotExist|123+" ), connectionMock.lastSentMessage );
    }
}
