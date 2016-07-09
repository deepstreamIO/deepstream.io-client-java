package io.deepstream.event;


import io.deepstream.ConnectionMock;
import io.deepstream.DeepstreamClient;
import io.deepstream.constants.Actions;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;
import io.deepstream.message.Message;
import io.deepstream.utils.Emitter;
import io.deepstream.utils.Util;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.net.URISyntaxException;
import java.util.Map;
import java.util.Properties;

import static org.mockito.Mockito.*;

public class EventHandlerTest {

    DeepstreamClient deepstreamClientMock;
    ConnectionMock connectionMock;
    EventHandler eventHandler;
    Emitter.Listener callbackMock;

    @Before
    public void setUp() throws URISyntaxException {
        callbackMock = mock( Emitter.Listener.class );
        connectionMock = new ConnectionMock();
        deepstreamClientMock = mock( DeepstreamClient.class );
        when( deepstreamClientMock.getConnectionState() ).thenReturn( ConnectionState.OPEN );

        Map options = new Properties();
        options.put( "subscriptionTimeout", "10" );

        eventHandler = new EventHandler( options, connectionMock, deepstreamClientMock );
    }

    @After
    public void tearDown() {

    }

    @Test
    public void emitsEventItHasNoListenersFor() {
        Assert.assertNull( connectionMock.lastSentMessage );
        eventHandler.emit( "myEvent", 8 );
        Assert.assertEquals( Util.convertChars("E|EVT|myEvent|N8+"), connectionMock.lastSentMessage );
    };

    @Test
    public void subscribesToEvent() {
        eventHandler.subscribe( "myEvent", callbackMock );
        Assert.assertEquals( Util.convertChars("E|S|myEvent+"), connectionMock.lastSentMessage );
    }

    @Test
    public void emitsErrorIfNotAckReceivedForSubscribe() throws InterruptedException {
        eventHandler.subscribe( "myEvent", callbackMock );
        Thread.sleep(30);
        verify( deepstreamClientMock, times(1) ).onError(Topic.EVENT, Event.ACK_TIMEOUT, "No ACK message received in time for SmyEvent");
    }

    @Test
    public void notifiesLocalListenersForEvents() throws InterruptedException {
        eventHandler.subscribe( "myEvent", callbackMock );
        eventHandler.emit( "myEvent", 8 );
        Thread.sleep(30);
        verify( callbackMock, times(1) ).call( new Object[] { 8 } );
    }

    @Test
    public void notifiesLocalListenersForRemoteEvents() throws InterruptedException {
        eventHandler.subscribe( "myEvent", callbackMock );
        eventHandler.handle( new Message(
                "raw",
                Topic.EVENT,
                Actions.EVENT,
                new String[] { "myEvent", "N23" }
        ));
        Thread.sleep(30);
        verify( callbackMock, times(1) ).call( (float) 23 );
    }

    @Test
    public void notifiesLocalListenersForRemoteEventsWithoutData() throws InterruptedException {
        eventHandler.subscribe( "myEvent", callbackMock );
        eventHandler.handle( new Message(
                "raw",
                Topic.EVENT,
                Actions.EVENT,
                new String[] { "myEvent" }
        ));
        verify( callbackMock, times(1) ).call( );
    }

    @Test
    public void emitsErrorIfEventDataIsNotTyped() throws InterruptedException {
        eventHandler.subscribe( "myEvent", callbackMock );
        eventHandler.handle( new Message(
                "raw",
                Topic.EVENT,
                Actions.ACK,
                new String[] { "S", "myEvent" }
        ));
        eventHandler.handle( new Message(
                "raw",
                Topic.EVENT,
                Actions.EVENT,
                new String[] { "myEvent", "notTyped" }
        ));
        verify( deepstreamClientMock, times(1) ).onError( Topic.ERROR, Event.MESSAGE_PARSE_ERROR, "UNKNOWN_TYPE (notTyped)" );
    }

    @Test
    public void removesLocalListeners() throws InterruptedException {
        eventHandler.subscribe( "myEvent", callbackMock );
        eventHandler.unsubscribe( "myEvent", callbackMock );
        eventHandler.emit( "myEvent", 11 );
        verify( callbackMock, times(0) ).call( 11 );
    }

    @Test
    public void emitsErrorIfNotAckReceivedForUnsubscribe() throws InterruptedException {
        eventHandler.subscribe( "myEvent", callbackMock );
        eventHandler.handle( new Message(
                "raw",
                Topic.EVENT,
                Actions.ACK,
                new String[] { "S", "myEvent" }
        ));
        eventHandler.unsubscribe( "myEvent", callbackMock );
        Thread.sleep(30);
        verify( deepstreamClientMock, times(1) ).onError(Topic.EVENT, Event.ACK_TIMEOUT, "No ACK message received in time for USmyEvent");
    }

    @Test
    public void emitsErrorForUnsolicitedMessage() throws InterruptedException {
        eventHandler.handle( new Message(
                "raw",
                Topic.EVENT,
                Actions.LISTEN,
                new String[] { "myEvent" }
        ));
        verify( deepstreamClientMock, times(1) ).onError( Topic.EVENT, Event.UNSOLICITED_MESSAGE, "myEvent" );
    }
}
