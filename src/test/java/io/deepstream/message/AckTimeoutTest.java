package io.deepstream.message;

import io.deepstream.DeepstreamClientMock;
import io.deepstream.ErrorCallback;
import io.deepstream.constants.Actions;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;
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

    ErrorCallback errorCallbackMock;
    DeepstreamClientMock deepstreamClientMock;
    AckTimeoutRegistry ackTimeoutRegistry;
    Message message;

    @Before
    public void setUp() throws URISyntaxException {
        this.errorCallbackMock = mock( ErrorCallback.class );
        this.deepstreamClientMock = new DeepstreamClientMock( this.errorCallbackMock );
        this.deepstreamClientMock.setConnectionState( ConnectionState.OPEN );

        this.ackTimeoutRegistry = new AckTimeoutRegistry( this.deepstreamClientMock, Topic.EVENT, 10 );
        message = new Message(null, null, null, new String[2]);
    }

    @After
    public void tearDown() {

    }

    @Test
    public void onTimeoutCalledWhenNoAckReceived() throws InterruptedException {
        ackTimeoutRegistry.add("Event1");
        Thread.sleep(15);
        verify(this.errorCallbackMock, times(1)).onError( Topic.EVENT, Event.ACK_TIMEOUT, "No ACK message received in time for Event1" );
    }

    @Test
    public void onTimeoutCalledWithActionWhenNoAckReceived() throws InterruptedException {
        ackTimeoutRegistry.add("Event1", Actions.SUBSCRIBE);
        Thread.sleep(15);
        verify(this.errorCallbackMock, times(1)).onError(  Topic.EVENT, Event.ACK_TIMEOUT, "No ACK message received in time for SEvent1" );
    }

    @Test
    public void onTimeoutNotCalledWhenAckReceived() throws InterruptedException {
        message.data[0] = Actions.SUBSCRIBE.toString();
        message.data[1] = "Event1";

        ackTimeoutRegistry.add( "Event1" );
        ackTimeoutRegistry.clear( message );
        Thread.sleep(15);
        verify(this.errorCallbackMock, times(0)).onError( Topic.EVENT, Event.ACK_TIMEOUT, "No ACK message received in time for Event1" );
    }

    @Test
    public void onTimeoutNotCalledWithActionWhenAckReceived() throws InterruptedException {
        message.data[0] = Actions.SUBSCRIBE.toString();
        message.data[1] = "Event1";

        ackTimeoutRegistry.add("Event1", Actions.SUBSCRIBE);
        ackTimeoutRegistry.clear( message );
        Thread.sleep(15);
        verify(this.errorCallbackMock, times(0)).onError( Topic.EVENT, Event.ACK_TIMEOUT, "No ACK message received in time for SEvent1" );
    }

    @Test
    public void acksNotSentUntilConnectionStateIsOpen() throws InterruptedException {
        deepstreamClientMock.setConnectionState( ConnectionState.CLOSED );

        ackTimeoutRegistry.add("Event1", Actions.SUBSCRIBE);
        Thread.sleep(15);
        verify(this.errorCallbackMock, times(0)).onError(  Topic.EVENT, Event.ACK_TIMEOUT, "No ACK message received in time for SEvent1" );

        deepstreamClientMock.setConnectionState( ConnectionState.OPEN);
        Thread.sleep(15);
        verify(this.errorCallbackMock, times(1)).onError(  Topic.EVENT, Event.ACK_TIMEOUT, "No ACK message received in time for SEvent1" );
    }

    @Test
    public void acksAddedTwiceOnlySentOnce() throws InterruptedException {
        ackTimeoutRegistry.add("Event1", Actions.SUBSCRIBE);
        ackTimeoutRegistry.add("Event1", Actions.SUBSCRIBE);
        Thread.sleep(15);
        verify(this.errorCallbackMock, times(1)).onError( Topic.EVENT, Event.ACK_TIMEOUT, "No ACK message received in time for SEvent1" );
    }
}