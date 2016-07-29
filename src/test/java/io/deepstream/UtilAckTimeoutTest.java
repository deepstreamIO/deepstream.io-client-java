package io.deepstream;

import io.deepstream.constants.Actions;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.net.URISyntaxException;

import static org.mockito.Mockito.*;

@RunWith( JUnit4.class )
public class UtilAckTimeoutTest {

    DeepstreamRuntimeErrorHandler deepstreamRuntimeErrorHandlerMock;
    DeepstreamClientMock deepstreamClientMock;
    UtilAckTimeoutRegistry ackTimeoutRegistry;
    Message message;

    @Before
    public void setUp() throws URISyntaxException {
        this.deepstreamRuntimeErrorHandlerMock = mock( DeepstreamRuntimeErrorHandler.class );
        this.deepstreamClientMock = new DeepstreamClientMock();
        this.deepstreamClientMock.setConnectionState( ConnectionState.OPEN );
        this.deepstreamClientMock.setRuntimeErrorHandler( this.deepstreamRuntimeErrorHandlerMock );
        ackTimeoutRegistry = new UtilAckTimeoutRegistry( this.deepstreamClientMock );
        message = new Message(null, null, null, new String[2]);
    }

    @After
    public void tearDown() {

    }

    @Test
    public void onTimeoutCalledWhenNoAckReceived() throws InterruptedException {
        ackTimeoutRegistry.add( Topic.EVENT, Actions.SUBSCRIBE, "Event1", 20 );
        Thread.sleep(50);
        verify(this.deepstreamRuntimeErrorHandlerMock, times(1)).onException(  Topic.EVENT, Event.ACK_TIMEOUT, "No ACK message received in time for SUBSCRIBE Event1" );
    }

    @Test
    public void onTimeoutNotCalledWhenAckReceived() throws InterruptedException {
        message.topic = Topic.EVENT;
        message.action = Actions.ACK;
        message.data[0] = Actions.SUBSCRIBE.toString();
        message.data[1] = "Event1";

        ackTimeoutRegistry.add( Topic.EVENT, Actions.SUBSCRIBE, "Event1", 20 );
        ackTimeoutRegistry.clear( message );
        Thread.sleep(50);
        verify(this.deepstreamRuntimeErrorHandlerMock, times(0)).onException( Topic.EVENT, Event.ACK_TIMEOUT, "No ACK message received in time for SUBSCRIBE Event1" );
    }

    @Test
    public void acksNotSentUntilConnectionStateIsOpen() throws InterruptedException {
        deepstreamClientMock.setConnectionState( ConnectionState.CLOSED );

        ackTimeoutRegistry.add( Topic.EVENT, Actions.SUBSCRIBE, "Event1", 20 );
        Thread.sleep(50);
        verify(this.deepstreamRuntimeErrorHandlerMock, times(0)).onException(  Topic.EVENT, Event.ACK_TIMEOUT, "No ACK message received in time for SUBSCRIBE Event1" );

        deepstreamClientMock.setConnectionState( ConnectionState.OPEN );
        Thread.sleep(50);
        verify(this.deepstreamRuntimeErrorHandlerMock, times(1)).onException(  Topic.EVENT, Event.ACK_TIMEOUT, "No ACK message received in time for SUBSCRIBE Event1" );
    }

    @Test
    public void acksAddedTwiceOnlySentOnce() throws InterruptedException {
        ackTimeoutRegistry.add( Topic.EVENT, Actions.SUBSCRIBE, "Event1", 20 );
        ackTimeoutRegistry.add( Topic.EVENT, Actions.SUBSCRIBE, "Event1", 20);
        Thread.sleep(50);
        verify(this.deepstreamRuntimeErrorHandlerMock, times(1)).onException( Topic.EVENT, Event.ACK_TIMEOUT, "No ACK message received in time for SUBSCRIBE Event1" );
    }
}