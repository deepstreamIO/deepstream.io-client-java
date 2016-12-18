package io.deepstream;


import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Properties;

import static org.mockito.Mockito.*;

public class ListTest {

    DeepstreamClientMock deepstreamClientMock;
    ConnectionMock connectionMock;
    RecordHandler recordHandler;
    DeepstreamRuntimeErrorHandler errorCallbackMock;
    List list;
    RecordEventsListener recordEventsListener;
    ListChangedListener listChangedListener;
    String listName = "someList";

    @Before
    public void setUp() throws InvalidDeepstreamConfig {

        this.connectionMock = new ConnectionMock();
        this.errorCallbackMock = mock( DeepstreamRuntimeErrorHandler.class );
        this.deepstreamClientMock = new DeepstreamClientMock();
        this.deepstreamClientMock.setRuntimeErrorHandler( errorCallbackMock );
        this.deepstreamClientMock.setConnectionState( ConnectionState.OPEN );

        Properties options = new Properties();
        options.put( "subscriptionTimeout", "10" );
        options.put( "recordDeleteTimeout", "10" );
        options.put( "recordReadAckTimeout", "10" );
        options.put( "recordReadTimeout", "20" );

        recordHandler = new RecordHandler( new DeepstreamConfig( options ), connectionMock, deepstreamClientMock );
        recordEventsListener = mock(RecordEventsListener.class);
        listChangedListener = mock( ListChangedListener.class);

        new Thread(new Runnable() {
            @Override
            public void run() {
                list = recordHandler.getList( listName );
                list.addRecordEventsListener(recordEventsListener);
                list.subscribe( listChangedListener );
            }
        }).start();

        try {
            Thread.sleep(50);
            recordHandler.handle( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|R|someList|1|[\"entryA\",\"entryB\"]" ), deepstreamClientMock ) );
            Thread.sleep(50);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    @After
    public void tearDown() {
    }

    @Test
    public void initialState() {
        String[] content = { "entryA", "entryB" };

        Assert.assertArrayEquals( list.getEntries(), content );
        //verify( listChangedListener, times( 1 ) ).onListChanged( listName, content );
        Assert.assertTrue( list.isReady() );
        Assert.assertFalse( list.isDestroyed() );
        Assert.assertFalse( list.isEmpty() );
    }

    @Test
    public void addsAnEntry() {
        initialState();

        reset( listChangedListener );

        String[] content = { "entryA", "entryB", "entryC" };

        list.addEntry( "entryC" );

        Assert.assertArrayEquals( list.getEntries(), content );
        verify( listChangedListener, times( 1 ) ).onListChanged( listName, content );

        Assert.assertEquals( connectionMock.lastSentMessage, TestUtil.replaceSeperators( "R|U|someList|2|[\"entryA\",\"entryB\",\"entryC\"]+" ) );
    }

    @Test
    public void removesAnEntry() {
        addsAnEntry();

        reset( listChangedListener );

        String[] content = { "entryA", "entryC" };

        list.removeEntry( "entryB" );

        Assert.assertArrayEquals( content, list.getEntries() );
        verify( listChangedListener, times( 1 ) ).onListChanged( listName, content );

        Assert.assertEquals( connectionMock.lastSentMessage, TestUtil.replaceSeperators( "R|U|someList|3|[\"entryA\",\"entryC\"]+" ) );
    }

    @Test
    public void addsAnEntryAtASpecificIndex() {
        removesAnEntry();

        reset( listChangedListener );

        String[] content = { "entryA", "entryD", "entryC" };
        list.addEntry( "entryD", 1 );

        Assert.assertArrayEquals( content, list.getEntries() );
        verify( listChangedListener, times( 1 ) ).onListChanged( listName, content );

        Assert.assertEquals( connectionMock.lastSentMessage, TestUtil.replaceSeperators( "R|U|someList|4|[\"entryA\",\"entryD\",\"entryC\"]+" ) );
    }

    @Test
    public void removesAnEntryAtASpecificIndex() {
        addsAnEntryAtASpecificIndex();

        reset( listChangedListener );

        String[] content = { "entryA", "entryC" };

        list.removeEntry( "entryD", 1 );

        Assert.assertArrayEquals( content, list.getEntries() );
        verify( listChangedListener, times( 1 ) ).onListChanged( listName, content );

        Assert.assertEquals( connectionMock.lastSentMessage, TestUtil.replaceSeperators( "R|U|someList|5|[\"entryA\",\"entryC\"]+" ) );
    }

    @Test
    public void setsTheEntireList() {
        removesAnEntryAtASpecificIndex();

        reset( listChangedListener );

        String[] content = { "u", "v" };
        list.setEntries( content );

        Assert.assertArrayEquals( content, list.getEntries() );
        verify( listChangedListener, times( 1 ) ).onListChanged( listName, content );

        Assert.assertEquals( connectionMock.lastSentMessage, TestUtil.replaceSeperators( "R|U|someList|6|[\"u\",\"v\"]+" ) );
    }

    @Test
    public void handlesServerUpdates() {
        setsTheEntireList();

        reset( listChangedListener );

        String[] content = { "x", "y" };

        recordHandler.handle( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|R|someList|7|[\"x\",\"y\"]" ), deepstreamClientMock ) );

        Assert.assertArrayEquals( content, list.getEntries() );
        verify( listChangedListener, times( 1 ) ).onListChanged( listName, content );
    }

    @Test
    public void handlesEmptyListUpdates() {
        handlesServerUpdates();

        reset( listChangedListener );

        String[] content = new String[] {};
        list.setEntries( content );

        Assert.assertEquals( connectionMock.lastSentMessage, TestUtil.replaceSeperators( "R|U|someList|8|[]+" ) );
        verify( listChangedListener, times( 1 ) ).onListChanged( listName, content );
        Assert.assertTrue( list.isEmpty() );
    }

    @Test
    public void canUnsubscribe() {
        handlesServerUpdates();

        reset( listChangedListener );

        list.unsubscribe( listChangedListener );

        String[] content = new String[] {};
        list.setEntries( content );

        verify( listChangedListener, times( 0 ) ).onListChanged( listName, content );
        Assert.assertEquals( connectionMock.lastSentMessage, TestUtil.replaceSeperators( "R|U|someList|8|[]+" ) );
    }

}
