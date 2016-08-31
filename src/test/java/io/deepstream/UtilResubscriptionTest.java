package io.deepstream;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.net.URISyntaxException;

import static org.mockito.Mockito.*;

@RunWith( JUnit4.class )
public class UtilResubscriptionTest {

    DeepstreamClientMock deepstreamClientMock;
    UtilResubscribeNotifier.UtilResubscribeListener resubscribeCallbackMock;
    UtilResubscribeNotifier resubscribeNotifier;
    DeepstreamRuntimeErrorHandler errorCallbackMock;

    @Before
    public void setUp() throws URISyntaxException {
        this.resubscribeCallbackMock = mock( UtilResubscribeNotifier.UtilResubscribeListener.class );

        this.errorCallbackMock = mock( DeepstreamRuntimeErrorHandler.class );
        this.deepstreamClientMock = new DeepstreamClientMock();
        this.deepstreamClientMock.setRuntimeErrorHandler(this.errorCallbackMock);
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