package io.deepstream.message;

import io.deepstream.ConnectionChangeListener;
import io.deepstream.DeepstreamClient;
import io.deepstream.DeepstreamClientMock;
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

import static org.mockito.Mockito.*;

@RunWith( JUnit4.class )
public class ResubscriptionTest {

    DeepstreamClientMock deepstreamClientMock;
    ResubscribeCallback resubscribeCallbackMock;
    ResubscribeNotifier resubscribeNotifier;

    @Before
    public void setUp() throws URISyntaxException {
        this.resubscribeCallbackMock = mock( ResubscribeCallback.class );
        this.deepstreamClientMock = new DeepstreamClientMock();

        this.resubscribeNotifier = new ResubscribeNotifier( this.deepstreamClientMock, this.resubscribeCallbackMock );
    }

    @After
    public void tearDown() {

    }

    @Test
    public void resubscribeCallbackNotCalledWhenReconnecting() {
        setConnectionState( ConnectionState.RECONNECTING );
        verify( resubscribeCallbackMock, times( 0 ) ).resubscribe();
    }

    @Test
    public void resubscribeCallbackNotCalledWhenOpening() {
        setConnectionState( ConnectionState.OPEN );
        verify( resubscribeCallbackMock, times( 0 ) ).resubscribe();
    }

    @Test
    public void resubscribeCallbackCalledWhenReconnectingAndOpen() {
        setConnectionState( ConnectionState.RECONNECTING );
        verify( resubscribeCallbackMock, times( 0 ) ).resubscribe();

        setConnectionState( ConnectionState.OPEN );
        verify( resubscribeCallbackMock, times( 1 ) ).resubscribe();
    }

    public void setConnectionState( ConnectionState state ) {
        for ( ConnectionChangeListener listener : this.deepstreamClientMock.connectionListeners ) {
            listener.connectionStateChanged( state );
        }
    }
}