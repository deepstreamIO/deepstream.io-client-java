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
    Message message;

    @Before
    public void setUp() throws URISyntaxException {
        this.deepstreamClientMock = new DeepstreamClientMock();
        this.deepstreamClientMock.setConnectionState( ConnectionState.OPEN );
        this.ackTimeoutCallback = mock( AckTimeoutCallback.class );
        this.ackTimeoutRegistry = new AckTimeoutRegistry(this.deepstreamClientMock, Topic.EVENT, 10, ackTimeoutCallback);
        message = new Message(null, null, null, new String[2]);
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

    @Test
    public void onTimeoutNotCalledWhenAckReceived() throws InterruptedException {
        message.data[0] = "Event1";

        ackTimeoutRegistry.add( "Event1" );
        ackTimeoutRegistry.clear( message );
        Thread.sleep(15);
        verify(ackTimeoutCallback, times(0)).onTimeout( "Event1" );
    }

    @Test
    public void onTimeoutNotCalledWithActionWhenAckReceived() throws InterruptedException {
        message.data[0] = Actions.SUBSCRIBE.toString();
        message.data[1] = "Event1";

        ackTimeoutRegistry.add("Event1", Actions.SUBSCRIBE);
        ackTimeoutRegistry.clear( message );
        Thread.sleep(15);
        verify(ackTimeoutCallback, times(0)).onTimeout( "SEvent1" );
    }

    @Test
    public void acksNotSentUntilConnectionStateIsOpen() throws InterruptedException {
        deepstreamClientMock.setConnectionState( ConnectionState.CLOSED );

        ackTimeoutRegistry.add("Event1", Actions.SUBSCRIBE);
        Thread.sleep(15);
        verify(ackTimeoutCallback, times(0)).onTimeout( "SEvent1" );

        deepstreamClientMock.setConnectionState( ConnectionState.OPEN);
        Thread.sleep(15);
        verify(ackTimeoutCallback, times(1)).onTimeout( "SEvent1" );
    }

    @Test
    public void acksAddedTwiceOnlySentOnce() throws InterruptedException {
        ackTimeoutRegistry.add("Event1", Actions.SUBSCRIBE);
        ackTimeoutRegistry.add("Event1", Actions.SUBSCRIBE);
        Thread.sleep(15);
        verify(ackTimeoutCallback, times(1)).onTimeout( "SEvent1" );
    }
}