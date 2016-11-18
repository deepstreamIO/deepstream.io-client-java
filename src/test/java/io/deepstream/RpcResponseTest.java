package io.deepstream;


import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.net.URISyntaxException;
import java.util.Properties;

import static org.mockito.Mockito.mock;

@RunWith( JUnit4.class )
public class RpcResponseTest {

    DeepstreamClient deepstreamClientMock;
    ConnectionMock connectionMock;
    RpcHandler rpcHandler;

    @Before
    public void setUp() throws URISyntaxException, InvalidDeepstreamConfig {
        this.deepstreamClientMock = mock(DeepstreamClient.class);
        this.connectionMock = new ConnectionMock();

        Properties options = new Properties();
        options.put("subscriptionTimeout", "2000");
        options.put("rpcAckTimeout", "6000");
        options.put("rpcResponseTimeout", "10000");

        this.rpcHandler = new RpcHandler( new DeepstreamConfig( options ), connectionMock, deepstreamClientMock);
    }

    @After
    public void tearDown() {
    }

    @Test
    public void sendsAckMessageAutomatically() {
        RpcResponse response = new RpcResponse( connectionMock, "addTwo", "123" );
        Assert.assertEquals(TestUtil.replaceSeperators("P|A|REQ|addTwo|123+"), connectionMock.lastSentMessage);
    }

    @Test
    public void sendsTheResponse() {
        RpcResponse response = new RpcResponse( connectionMock, "addTwo", "123" );
        response.send( 14 );
        Assert.assertEquals(TestUtil.replaceSeperators("P|RES|addTwo|123|N14+"), connectionMock.lastSentMessage);
    }

    @Test
    public void rejectsTheMessage() {
        RpcResponse response = new RpcResponse( connectionMock, "addTwo", "123" );
        response.reject();
        Assert.assertEquals(TestUtil.replaceSeperators("P|REJ|addTwo|123+"), connectionMock.lastSentMessage);
    }

    @Test
    public void throwsWhenSendingRejectedMessage() {
        RpcResponse response = new RpcResponse( connectionMock, "addTwo", "123" );
        response.reject();
        try {
            response.send( "bla" );
        } catch ( DeepstreamException ex ) {
            Assert.assertTrue( ex.getMessage().contains( "Rpc addTwo already completed" ));
        }
    }

    @Test
    public void errorsTheMessage() {
        RpcResponse response = new RpcResponse( connectionMock, "addTwo", "123" );
        response.error( "Error Message" );
        Assert.assertEquals(TestUtil.replaceSeperators("P|E|Error Message|addTwo|123+"), connectionMock.lastSentMessage);
    }

    @Test
    public void throwsWhenSendingErroredMessage() {
        RpcResponse response = new RpcResponse( connectionMock, "addTwo", "123" );
        response.error( "Err msg" );
        try {
            response.send( "bla" );
        } catch ( DeepstreamException ex ) {
            Assert.assertTrue( ex.getMessage().contains( "Rpc addTwo already completed" ));
        }
    }
}