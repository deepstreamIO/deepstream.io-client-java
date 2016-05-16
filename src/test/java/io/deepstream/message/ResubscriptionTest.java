package io.deepstream.message;

import io.deepstream.DeepstreamClient;
import io.deepstream.constants.ConnectionState;
import io.deepstream.utils.ResubscribeCallback;
import io.deepstream.utils.ResubscribeNotifier;
import io.socket.engineio.client.Socket;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.Mockito.*;

@RunWith( JUnit4.class )
public class ResubscriptionTest {

    String originalUrl = "originalProtocol://originalHost:originalPort";

    Socket socketMock;
    DeepstreamClient deepstreamClient;
    ResubscribeCallback resubscribeCallbackMock;
    ResubscribeNotifier resubscribeNotifier;
    Connection connection;

    @Before
    public void setUp() throws URISyntaxException {
        this.resubscribeCallbackMock = mock( ResubscribeCallback.class );
        this.socketMock = new Socket(originalUrl);
        this.connection = new Connection(originalUrl, new HashMap(), this.deepstreamClient, this.socketMock);
        this.deepstreamClient = new DeepstreamClient( this.connection, new HashMap() );

        this.resubscribeNotifier = new ResubscribeNotifier( this.deepstreamClient, this.resubscribeCallbackMock );
    }

    @After
    public void tearDown() {

    }

    @Test
    public void resubscribeCallbackNotCalledWhenReconnecting() {
        this.connection.setState( ConnectionState.RECONNECTING );
        verify( resubscribeCallbackMock, times( 0 ) ).call();
    }

    @Test
    public void resubscribeCallbackNotCalledWhenOpening() {
        this.connection.setState( ConnectionState.OPEN );
        verify( resubscribeCallbackMock, times( 0 ) ).call();
    }

    @Test
    public void resubscribeCallbackCalledWhenReconnectingAndOpen() {
        this.connection.setState( ConnectionState.RECONNECTING );
        verify( resubscribeCallbackMock, times( 0 ) ).call();

        this.connection.setState( ConnectionState.OPEN );
        verify( resubscribeCallbackMock, times( 1 ) ).call();
    }
}