package io.deepstream.message;

import io.deepstream.DeepstreamClientMock;
import io.deepstream.constants.Actions;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Topic;
import io.deepstream.utils.AckTimeoutCallback;
import io.deepstream.utils.AckTimeoutRegistry;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.net.URISyntaxException;

import static org.mockito.Mockito.*;

@RunWith( JUnit4.class )
public class AckTimeoutTest {

    DeepstreamClientMock deepstreamClientMock;
    AckTimeoutCallback ackTimeoutCallback;
    AckTimeoutRegistry ackTimeoutRegistry;

    @Before
    public void setUp() throws URISyntaxException {
        this.deepstreamClientMock = new DeepstreamClientMock();
        this.deepstreamClientMock.setConnectionState( ConnectionState.OPEN );
        this.ackTimeoutCallback = mock( AckTimeoutCallback.class );
        this.ackTimeoutRegistry = new AckTimeoutRegistry(this.deepstreamClientMock, Topic.EVENT, 10, ackTimeoutCallback);
    }

    @After
    public void tearDown() {

    }

    @Test
    public void onTimeoutCalledWhenNoAckReceived() throws InterruptedException {
        ackTimeoutRegistry.add("Event1");
        Thread.sleep(15);
        verify(ackTimeoutCallback, times(1)).onTimeout( "Event1" );
    }

    @Test
    public void onTimeoutCalledWithActionWhenNoAckReceived() throws InterruptedException {
        ackTimeoutRegistry.add("Event1", Actions.SUBSCRIBE);
        Thread.sleep(15);
        verify(ackTimeoutCallback, times(1)).onTimeout( "SEvent1" );
    }
}