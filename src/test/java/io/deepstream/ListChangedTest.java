package io.deepstream;


import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Properties;

import static org.mockito.Mockito.*;

public class ListChangedTest {
    DeepstreamClientMock deepstreamClientMock;
    ConnectionMock connectionMock;
    RecordHandler recordHandler;
    DeepstreamRuntimeErrorHandler errorCallbackMock;
    List list;
    RecordEventsListener recordEventsListener;
    ListChangedListener listChangedListener;
    ListEntryChangedListener listEntryChangedListener;
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
        listEntryChangedListener = mock( ListEntryChangedListener.class);

        new Thread(new Runnable() {
            @Override
            public void run() {
                list = recordHandler.getList( listName );
                list.addRecordEventsListener(recordEventsListener);
                list.subscribe( listChangedListener );
                list.subscribe( listEntryChangedListener );
            }
        }).start();

        try {
            Thread.sleep(300);
            recordHandler.handle( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|R|someList|1|[\"a\",\"b\",\"c\",\"d\",\"e\"]" ), deepstreamClientMock ) );
            Thread.sleep(300);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    @After
    public void tearDown() {
    }

    @Test
    public void entryAddedLocallyTest() {
        list.addEntry( "f" );
        verify( listEntryChangedListener, times( 1 ) ).onEntryAdded( listName, "f", 5 );
    }

    @Test
    public void entryAddedLocallyWithIndexTest() {
        list.addEntry( "f", 3 );
        verify( listEntryChangedListener, times( 1 ) ).onEntryAdded( listName, "f", 3 );
    }

    @Test
    public void entryAddedRemotelyTest() {
        recordHandler.handle( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|R|someList|2|[\"a\",\"b\",\"c\",\"d\",\"e\",\"f\"]" ), deepstreamClientMock ) );
        verify( listEntryChangedListener, times( 1 ) ).onEntryAdded( listName, "f", 5 );
    }

    @Test
    public void entryRemovedLocallyTest() {
        list.removeEntry( "c" );
        verify( listEntryChangedListener, times( 0 ) ).onEntryAdded( anyString(), anyString(), anyInt() );
        verify( listEntryChangedListener, times( 1 ) ).onEntryRemoved( listName, "c", 2 );
    }

    @Test
    public void entryRemovedLocallyWithIndexTest() {
        list.removeEntry( "c", 2 );
        verify( listEntryChangedListener, times( 0 ) ).onEntryAdded( anyString(), anyString(), anyInt() );
        verify( listEntryChangedListener, times( 1 ) ).onEntryRemoved( listName, "c", 2 );
    }

    @Test
    public void entryMoved() {
        ArrayList entries = new ArrayList();
        entries.add( "a" );
        entries.add( "b" );
        entries.add( "e" );
        entries.add( "d" );
        entries.add( "c" );

        list.setEntries( entries );

        verify( listEntryChangedListener, times( 0 ) ).onEntryAdded( anyString(), anyString(), anyInt() );
        verify( listEntryChangedListener, times( 0 ) ).onEntryRemoved( anyString(), anyString(), anyInt() );

        verify( listEntryChangedListener, times( 1 ) ).onEntryMoved( listName, "e", 2 );
        verify( listEntryChangedListener, times( 1 ) ).onEntryMoved( listName, "c", 4 );
    }

    //TODO
    //@Test
    public void notifiesWhenAnotherInstanceOfSameItemIsAddedWithIndex() {
        list.addEntry( "a", 3 );

        verify( listEntryChangedListener, times( 1 ) ).onEntryAdded( listName, "a", 3 );
        verify( listEntryChangedListener, times( 0 ) ).onEntryRemoved( anyString(), anyString(), anyInt() );

        verify( listEntryChangedListener, times( 1 ) ).onEntryMoved(  listName, "d" , 4 );
        verify( listEntryChangedListener, times( 1 ) ).onEntryMoved(  listName, "e" , 5 );
    }

    //TODO
    //@Test
    public void notifiesWhenAnotherInstanceOfSameItemIsAddedWithoutIndex() {
        list.addEntry( "b" );

        verify( listEntryChangedListener, times( 1 ) ).onEntryAdded( listName, "b", 5 );
        verify( listEntryChangedListener, times( 0 ) ).onEntryRemoved( anyString(), anyString(), anyInt() );
        verify( listEntryChangedListener, times( 0 ) ).onEntryMoved( anyString(), anyString(), anyInt() );
    }

    @Test
    public void notifiesWhenSecondInstanceOfSameItemIsRemoved() {
        ArrayList entries = new ArrayList();
        entries.add( "a" );
        entries.add( "d" );
        entries.add( "b" );
        entries.add( "c" );

        list.setEntries( entries );

        verify( listEntryChangedListener, times( 0 ) ).onEntryAdded( anyString(), anyString(), anyInt() );

        verify( listEntryChangedListener, times( 1 ) ).onEntryMoved( listName, "d", 1 );
        verify( listEntryChangedListener, times( 1 ) ).onEntryMoved( listName, "b", 2 );
        verify( listEntryChangedListener, times( 1 ) ).onEntryMoved( listName, "c", 3 );

        verify( listEntryChangedListener, times( 1 ) ).onEntryRemoved( listName, "e", 4 );
    }

    //TODO
    //@Test
    public void notifiesTheListenerForAnAddMoveCombination() {
        ArrayList entries = new ArrayList();
        entries.add( "a" );
        entries.add( "b" );
        entries.add( "c" );
        entries.add( "c" );
        entries.add( "d" );
        entries.add( "e" );

        list.setEntries( entries );

        verify( listEntryChangedListener, times( 1 ) ).onEntryAdded( listName, "c", 3 );

        verify( listEntryChangedListener, times( 1 ) ).onEntryMoved( listName, "d", 4 );
        verify( listEntryChangedListener, times( 1 ) ).onEntryMoved( listName, "b", 5 );

        verify( listEntryChangedListener, times( 0 ) ).onEntryRemoved( anyString(), anyString(), anyInt() );
    }

    @Test
    public void notifiesTheListenerForAnAddMoveRemoveCombination() {
        ArrayList entries = new ArrayList();
        entries.add( "c" );
        entries.add( "b" );
        entries.add( "f" );

        list.setEntries( entries );

        verify( listEntryChangedListener, times( 1 ) ).onEntryAdded( listName, "f", 2 );

        verify( listEntryChangedListener, times( 1 ) ).onEntryMoved( listName, "c", 0 );

        verify( listEntryChangedListener, times( 1 ) ).onEntryRemoved( listName, "a", 0 );
        verify( listEntryChangedListener, times( 1 ) ).onEntryRemoved( listName, "d", 3 );
        verify( listEntryChangedListener, times( 1 ) ).onEntryRemoved( listName, "e", 4 );
    }


}
