package io.deepstream;


import io.deepstream.constants.ConnectionState;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Map;
import java.util.Properties;

import static org.mockito.Mockito.*;

public class ListChangedTest {

    Map options;
    DeepstreamClientMock deepstreamClientMock;
    ConnectionMock connectionMock;
    RecordHandler recordHandler;
    DeepstreamRuntimeErrorHandler errorCallbackMock;
    List list;
    RecordEventsListener recordEventsListener;
    ListReadyListener listReadyListener;
    ListChangedListener listChangedListener;
    String listName = "someList";

    @Before
    public void setUp() {

        this.connectionMock = new ConnectionMock();
        this.errorCallbackMock = mock( DeepstreamRuntimeErrorHandler.class );
        this.deepstreamClientMock = new DeepstreamClientMock();
        this.deepstreamClientMock.setRuntimeErrorHandler( errorCallbackMock );
        this.deepstreamClientMock.setConnectionState( ConnectionState.OPEN );

        options = new Properties();
        options.put( "subscriptionTimeout", "10" );
        options.put( "recordDeleteTimeout", "10" );
        options.put( "recordReadAckTimeout", "10" );
        options.put( "recordReadTimeout", "20" );

        recordHandler = new RecordHandler( options, connectionMock, deepstreamClientMock );
        recordEventsListener = mock(RecordEventsListener.class);
        listReadyListener = mock(ListReadyListener.class);
        listChangedListener = mock( ListChangedListener.class);

        list = recordHandler.getList( listName );
        list.addRecordEventsListener(recordEventsListener);
        list.addListReadyListener( listReadyListener );

        recordHandler.handle( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|R|someList|1|[\"a\",\"b\",\"c\",\"d\",\"e\"]" ), deepstreamClientMock ) );

        list.subscribe( listChangedListener );
    }

    @After
    public void tearDown() {
    }

    @Test
    public void entryAddedLocallyTest() {
        list.addEntry( "f" );
        verify( listChangedListener, times( 1 ) ).onEntryAdded( listName, "f", 5 );
    }

    @Test
    public void entryAddedLocallyWithIndexTest() {
        list.addEntry( "f", 3 );
        verify( listChangedListener, times( 1 ) ).onEntryAdded( listName, "f", 3 );
    }

    @Test
    public void entryAddedRemotelyTest() {
        recordHandler.handle( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|R|someList|2|[\"a\",\"b\",\"c\",\"d\",\"e\",\"f\"]" ), deepstreamClientMock ) );
        verify( listChangedListener, times( 1 ) ).onEntryAdded( listName, "f", 5 );
    }

    @Test
    public void entryRemovedLocallyTest() {
        list.removeEntry( "c" );
        verify( listChangedListener, times( 0 ) ).onEntryAdded( anyString(), anyString(), anyInt() );
        verify( listChangedListener, times( 1 ) ).onEntryRemoved( listName, "c", 2 );
    }

    @Test
    public void entryRemovedLocallyWithIndexTest() {
        list.removeEntry( "c", 2 );
        verify( listChangedListener, times( 0 ) ).onEntryAdded( anyString(), anyString(), anyInt() );
        verify( listChangedListener, times( 1 ) ).onEntryRemoved( listName, "c", 2 );
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

        verify( listChangedListener, times( 0 ) ).onEntryAdded( anyString(), anyString(), anyInt() );
        verify( listChangedListener, times( 0 ) ).onEntryRemoved( anyString(), anyString(), anyInt() );

        verify( listChangedListener, times( 1 ) ).onEntryMoved( listName, "e", 2 );
        verify( listChangedListener, times( 1 ) ).onEntryMoved( listName, "c", 4 );
    }

    //TODO
    //@Test
    public void notifiesWhenAnotherInstanceOfSameItemIsAddedWithIndex() {
        list.addEntry( "a", 3 );

        verify( listChangedListener, times( 1 ) ).onEntryAdded( listName, "a", 3 );
        verify( listChangedListener, times( 0 ) ).onEntryRemoved( anyString(), anyString(), anyInt() );

        verify( listChangedListener, times( 1 ) ).onEntryMoved(  listName, "d" , 4 );
        verify( listChangedListener, times( 1 ) ).onEntryMoved(  listName, "e" , 5 );
    }

    //TODO
    //@Test
    public void notifiesWhenAnotherInstanceOfSameItemIsAddedWithoutIndex() {
        list.addEntry( "b" );

        verify( listChangedListener, times( 1 ) ).onEntryAdded( listName, "b", 5 );
        verify( listChangedListener, times( 0 ) ).onEntryRemoved( anyString(), anyString(), anyInt() );
        verify( listChangedListener, times( 0 ) ).onEntryMoved( anyString(), anyString(), anyInt() );
    }

    @Test
    public void notifiesWhenSecondInstanceOfSameItemIsRemoved() {
        ArrayList entries = new ArrayList();
        entries.add( "a" );
        entries.add( "d" );
        entries.add( "b" );
        entries.add( "c" );

        list.setEntries( entries );

        verify( listChangedListener, times( 0 ) ).onEntryAdded( anyString(), anyString(), anyInt() );

        verify( listChangedListener, times( 1 ) ).onEntryMoved( listName, "d", 1 );
        verify( listChangedListener, times( 1 ) ).onEntryMoved( listName, "b", 2 );
        verify( listChangedListener, times( 1 ) ).onEntryMoved( listName, "c", 3 );

        verify( listChangedListener, times( 1 ) ).onEntryRemoved( listName, "e", 4 );
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

        verify( listChangedListener, times( 1 ) ).onEntryAdded( listName, "c", 3 );

        verify( listChangedListener, times( 1 ) ).onEntryMoved( listName, "d", 4 );
        verify( listChangedListener, times( 1 ) ).onEntryMoved( listName, "b", 5 );

        verify( listChangedListener, times( 0 ) ).onEntryRemoved( anyString(), anyString(), anyInt() );
    }

    @Test
    public void notifiesTheListenerForAnAddMoveRemoveCombination() {
        ArrayList entries = new ArrayList();
        entries.add( "c" );
        entries.add( "b" );
        entries.add( "f" );

        list.setEntries( entries );

        verify( listChangedListener, times( 1 ) ).onEntryAdded( listName, "f", 2 );

        verify( listChangedListener, times( 1 ) ).onEntryMoved( listName, "c", 0 );

        verify( listChangedListener, times( 1 ) ).onEntryRemoved( listName, "a", 0 );
        verify( listChangedListener, times( 1 ) ).onEntryRemoved( listName, "d", 3 );
        verify( listChangedListener, times( 1 ) ).onEntryRemoved( listName, "e", 4 );
    }


}
