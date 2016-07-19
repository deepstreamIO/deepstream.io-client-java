package io.deepstream;


import io.deepstream.constants.Actions;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.net.URISyntaxException;
import java.util.Map;
import java.util.Properties;

import static org.mockito.Mockito.*;

public class EventHandlerTest {

    DeepstreamClientMock deepstreamClientMock;
    ConnectionMock connectionMock;
    EventHandler eventHandler;
    Emitter.Listener callbackMock;
    ErrorCallback errorCallbackMock;

    @Before
    public void setUp() throws URISyntaxException {
        callbackMock = mock( Emitter.Listener.class );

        this.connectionMock = new ConnectionMock();
        this.errorCallbackMock = mock( ErrorCallback.class );
        this.deepstreamClientMock = new DeepstreamClientMock( this.errorCallbackMock );
        this.deepstreamClientMock.setConnectionState( ConnectionState.OPEN );

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
        Assert.assertEquals( TestUtil.replaceSeperators("E|EVT|myEvent|N8+"), connectionMock.lastSentMessage );
    };

    @Test
    public void subscribesToEvent() {
        eventHandler.subscribe( "myEvent", callbackMock );
        Assert.assertEquals( TestUtil.replaceSeperators("E|S|myEvent+"), connectionMock.lastSentMessage );
    }

    @Test
    public void emitsErrorIfNotAckReceivedForSubscribe() throws InterruptedException {
        eventHandler.subscribe( "myEvent", callbackMock );
        Thread.sleep(50);
        verify( errorCallbackMock, times(1) ).onError(Topic.EVENT, Event.ACK_TIMEOUT, "No ACK message received in time for SUBSCRIBE myEvent");
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
        verify( errorCallbackMock, times(1) ).onError( Topic.ERROR, Event.MESSAGE_PARSE_ERROR, "UNKNOWN_TYPE (notTyped)" );
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
        verify( errorCallbackMock, times(1) ).onError(Topic.EVENT, Event.ACK_TIMEOUT, "No ACK message received in time for UNSUBSCRIBE myEvent");
    }

    @Test
    public void emitsErrorForUnsolicitedMessage() throws InterruptedException {
        eventHandler.handle( new Message(
                "raw",
                Topic.EVENT,
                Actions.LISTEN,
                new String[] { "myEvent" }
        ));
        verify( errorCallbackMock, times(1) ).onError( Topic.EVENT, Event.UNSOLICITED_MESSAGE, "myEvent" );
    }
}
