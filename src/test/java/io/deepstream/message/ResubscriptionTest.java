package io.deepstream.message;

import io.deepstream.DeepstreamClientMock;
import io.deepstream.constants.ConnectionState;
import io.deepstream.utils.ResubscribeCallback;
import io.deepstream.utils.ResubscribeNotifier;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.net.URISyntaxException;

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
        deepstreamClientMock.setConnectionState( ConnectionState.RECONNECTING );
        verify( resubscribeCallbackMock, times( 0 ) ).resubscribe();
    }

    @Test
    public void resubscribeCallbackNotCalledWhenOpening() {
        deepstreamClientMock.setConnectionState( ConnectionState.OPEN );
        verify( resubscribeCallbackMock, times( 0 ) ).resubscribe();
    }

    @Test
    public void resubscribeCallbackCalledWhenReconnectingAndOpen() {
        deepstreamClientMock.setConnectionState( ConnectionState.RECONNECTING );
        verify( resubscribeCallbackMock, times( 0 ) ).resubscribe();

        deepstreamClientMock.setConnectionState( ConnectionState.OPEN );
        verify( resubscribeCallbackMock, times( 1 ) ).resubscribe();
    }


}