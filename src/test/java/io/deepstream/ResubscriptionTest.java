package io.deepstream;

import io.deepstream.constants.ConnectionState;
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
    UtilResubscribeCallback resubscribeCallbackMock;
    UtilResubscribeNotifier resubscribeNotifier;
    ErrorCallback errorCallbackMock;

    @Before
    public void setUp() throws URISyntaxException {
        this.resubscribeCallbackMock = mock( UtilResubscribeCallback.class );

        this.errorCallbackMock = mock( ErrorCallback.class );
        this.deepstreamClientMock = new DeepstreamClientMock( this.errorCallbackMock );
        this.deepstreamClientMock.setConnectionState( ConnectionState.OPEN );

        this.resubscribeNotifier = new UtilResubscribeNotifier( this.deepstreamClientMock, this.resubscribeCallbackMock );
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