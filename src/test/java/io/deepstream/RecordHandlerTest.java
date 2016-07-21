package io.deepstream;


import io.deepstream.constants.ConnectionState;
import org.junit.After;
import org.junit.Before;

import java.util.Map;
import java.util.Properties;

import static org.mockito.Mockito.mock;

public class RecordHandlerTest {

    DeepstreamClientMock deepstreamClientMock;
    ConnectionMock connectionMock;
    RecordHandler recordHandler;
    ErrorCallback errorCallbackMock;

    @Before
    public void setUp() {

        this.connectionMock = new ConnectionMock();
        this.errorCallbackMock = mock( ErrorCallback.class );
        this.deepstreamClientMock = new DeepstreamClientMock( this.errorCallbackMock );
        this.deepstreamClientMock.setConnectionState( ConnectionState.OPEN );

        Map options = new Properties();
        options.put( "subscriptionTimeout", "10" );

        recordHandler = new RecordHandler( options, connectionMock, deepstreamClientMock );
    }

    @After
    public void tearDown() {

    }
}
