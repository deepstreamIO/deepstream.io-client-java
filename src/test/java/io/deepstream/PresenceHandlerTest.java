package io.deepstream;


import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.net.URISyntaxException;
import java.util.Properties;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

public class PresenceHandlerTest {

    DeepstreamClientMock deepstreamClientMock;
    ConnectionMock connectionMock;
    PresenceHandler presenceHandler;
    PresenceListener queryCallback;
    PresenceEventListener presenceCallback;
    DeepstreamRuntimeErrorHandler deepstreamRuntimeErrorHandler;

    @Before
    public void setUp() throws URISyntaxException, InvalidDeepstreamConfig {
        queryCallback = mock( PresenceListener.class );
        presenceCallback = mock( PresenceEventListener.class );
        this.connectionMock = new ConnectionMock();
        this.deepstreamRuntimeErrorHandler = mock( DeepstreamRuntimeErrorHandler.class );
        this.deepstreamClientMock = new DeepstreamClientMock();
        this.deepstreamClientMock.setRuntimeErrorHandler( this.deepstreamRuntimeErrorHandler );
        this.deepstreamClientMock.setConnectionState( ConnectionState.OPEN );

        Properties options = new Properties();
        options.put( "subscriptionTimeout", "10" );

        presenceHandler = new PresenceHandler( new DeepstreamConfig( options ), connectionMock, deepstreamClientMock );
    }

    @After
    public void tearDown() {

    }

    @Test
    public void queriesForConnectedClientsAndReceiveEmptyArray() throws DeepstreamError {
        Assert.assertNull( connectionMock.lastSentMessage );
        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    Thread.sleep(100);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                presenceHandler.handle( new Message(
                        "raw",
                        Topic.PRESENCE,
                        Actions.QUERY,
                        new String[]{}
                ));
            }
        }).start();
        presenceHandler.getAll( queryCallback );
        Assert.assertEquals( TestUtil.replaceSeperators("U|Q|Q+"), connectionMock.lastSentMessage );
        verify( queryCallback, times(1) ).onClients( new String[]{} );
    }

    @Test
    public void queriesForClientsAndReceivesClientArray() throws InterruptedException, DeepstreamError {
        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    Thread.sleep(100);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                presenceHandler.handle( new Message(
                        "raw",
                        Topic.PRESENCE,
                        Actions.QUERY,
                        new String[]{ "Bart", "Homer" }
                ));
            }
        }).start();
        presenceHandler.getAll( queryCallback );
        Assert.assertEquals( TestUtil.replaceSeperators("U|Q|Q+"), connectionMock.lastSentMessage );
        verify( queryCallback, times(1) ).onClients( new String[]{ "Bart", "Homer" } );
    }

    @Test
    public void subscribesToLoginsAndIsAlertedWhenClientLogsIn() throws InterruptedException {
        presenceHandler.subscribe( presenceCallback );
        Assert.assertEquals( TestUtil.replaceSeperators("U|S|S+"), connectionMock.lastSentMessage );
        presenceHandler.handle( new Message(
                "raw",
                Topic.PRESENCE,
                Actions.PRESENCE_JOIN,
                new String[] { "Homer" }
        ));
        verify( presenceCallback, times(1) ).onClientLogin( "Homer" );
    }

    @Test
    public void unsubscribesToLoginsAndIsNotAlertedWhenClientLogsIn() throws InterruptedException {
        presenceHandler.unsubscribe( presenceCallback );
        Assert.assertEquals( TestUtil.replaceSeperators("U|US|US+"), connectionMock.lastSentMessage );
        presenceHandler.handle( new Message(
                "raw",
                Topic.PRESENCE,
                Actions.PRESENCE_JOIN,
                new String[] { "Homer" }
        ));
        verify( presenceCallback, times(0) ).onClientLogin( "Homer" );
    }
}
