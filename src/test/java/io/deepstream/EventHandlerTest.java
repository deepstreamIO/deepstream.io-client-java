package io.deepstream;


import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.net.URISyntaxException;
import java.util.Properties;

import static org.mockito.Mockito.*;

public class EventHandlerTest {

    DeepstreamClientMock deepstreamClientMock;
    ConnectionMock connectionMock;
    EventHandler eventHandler;
    EventListener callbackMock;
    DeepstreamRuntimeErrorHandler deepstreamRuntimeErrorHandler;

    @Before
    public void setUp() throws URISyntaxException, InvalidDeepstreamConfig {
        callbackMock = mock( EventListener.class );

        this.connectionMock = new ConnectionMock();
        this.deepstreamRuntimeErrorHandler = mock( DeepstreamRuntimeErrorHandler.class );
        this.deepstreamClientMock = new DeepstreamClientMock();
        this.deepstreamClientMock.setRuntimeErrorHandler( this.deepstreamRuntimeErrorHandler );
        this.deepstreamClientMock.setConnectionState( ConnectionState.OPEN );

        Properties options = new Properties();
        options.put( "subscriptionTimeout", "10" );

        eventHandler = new EventHandler( new DeepstreamConfig( options ), connectionMock, deepstreamClientMock );
    }

    @After
    public void tearDown() {

    }

    @Test
    public void emitsEventItHasNoListenersFor() {
        Assert.assertNull( connectionMock.lastSentMessage );
        eventHandler.emit( "myEvent", 8 );
        Assert.assertEquals( TestUtil.replaceSeperators("E|EVT|myEvent|N8+"), connectionMock.lastSentMessage );
    }

    @Test
    public void subscribesToEvent() {
        eventHandler.subscribe( "myEvent", callbackMock );
        Assert.assertEquals( TestUtil.replaceSeperators("E|S|myEvent+"), connectionMock.lastSentMessage );
    }

    @Test
    public void emitsErrorIfNotAckReceivedForSubscribe() throws InterruptedException {
        eventHandler.subscribe( "myEvent", callbackMock );
        Thread.sleep(50);
        verify( deepstreamRuntimeErrorHandler, times(1) ).onException(Topic.EVENT, Event.ACK_TIMEOUT, "No ACK message received in time for SUBSCRIBE myEvent");
    }

    @Test
    public void notifiesLocalListenersForEvents() throws InterruptedException {
        eventHandler.subscribe( "myEvent", callbackMock );
        eventHandler.emit( "myEvent", 8 );
        Thread.sleep(30);
        verify( callbackMock, times(1) ).onEvent( "myEvent", 8);
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
        verify( callbackMock, times(1) ).onEvent( "myEvent", (float) 23 );
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
        verify( callbackMock, times(1) ).onEvent( "myEvent" );
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
        verify( deepstreamRuntimeErrorHandler, times(1) ).onException( Topic.ERROR, Event.MESSAGE_PARSE_ERROR, "UNKNOWN_TYPE (notTyped)" );
    }

    @Test
    public void removesLocalListeners() throws InterruptedException {
        eventHandler.subscribe( "myEvent", callbackMock );
        eventHandler.unsubscribe( "myEvent", callbackMock );
        eventHandler.emit( "myEvent", 11 );
        verify( callbackMock, times(0) ).onEvent( "myEvent", 11 );
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
        Thread.sleep(50);
        verify( deepstreamRuntimeErrorHandler, times(1) ).onException(Topic.EVENT, Event.ACK_TIMEOUT, "No ACK message received in time for UNSUBSCRIBE myEvent");
    }

    @Test
    public void emitsErrorForUnsolicitedMessage() throws InterruptedException {
        eventHandler.handle( new Message(
                "raw",
                Topic.EVENT,
                Actions.LISTEN,
                new String[] { "myEvent" }
        ));
        verify( deepstreamRuntimeErrorHandler, times(1) ).onException( Topic.EVENT, Event.UNSOLICITED_MESSAGE, "myEvent" );
    }
}
