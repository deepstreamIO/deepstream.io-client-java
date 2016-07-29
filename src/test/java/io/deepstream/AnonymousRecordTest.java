package io.deepstream;


import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;
import io.deepstream.constants.ConnectionState;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Map;
import java.util.Properties;

import static org.mockito.Mockito.*;

public class AnonymousRecordTest {

    Gson gson = new Gson();
    Map options;
    DeepstreamClientMock deepstreamClientMock;
    ConnectionMock connectionMock;
    RecordHandler recordHandler;
    DeepstreamRuntimeErrorHandler errorCallbackMock;
    AnonymousRecord anonymousRecord;
    RecordEventsListener recordEventsListener;
    AnonymousRecordNameChangedListener recordNameChangedListener;
    RecordChangedCallback recordChangedCallback;
    String firstRecordName = "firstRecordName";
    String secondRecordName = "secondRecordName";
    String thirdRecordName = "thirdRecordNames";

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
        recordChangedCallback = mock(RecordChangedCallback.class);
        recordEventsListener = mock(RecordEventsListener.class);
        recordNameChangedListener = mock(AnonymousRecordNameChangedListener.class);
    }

    @After
    public void tearDown() {
    }

    @Test
    public void recordHasCorrectDefaultState() {
        anonymousRecord = new AnonymousRecord( this.recordHandler );
        anonymousRecord.addRecordEventsListener(recordEventsListener);
        anonymousRecord.addRecordNameChangedListener( recordNameChangedListener );
    }

    @Test
    public void worksBeforeSetNameIsCalled() {
        recordHasCorrectDefaultState();

        Assert.assertEquals( anonymousRecord.get(), null );
        Assert.assertEquals( anonymousRecord.name, null );

        anonymousRecord.addRecordEventsListener(recordEventsListener);
        anonymousRecord.subscribe( recordChangedCallback );
        anonymousRecord.subscribe( "firstname", recordChangedCallback );

        verify( recordEventsListener, times( 0 )).onRecordReady( any( Record.class ) ); //TODO
        verify( recordEventsListener, times( 0 )).onRecordDeleted( anyString() );
        verify( recordEventsListener, times( 0 )).onRecordDiscarded( anyString() );
        verify( recordChangedCallback, times( 0) ).onRecordChanged( anyString(), any(JsonElement.class));
        verify( recordChangedCallback, times( 0) ).onRecordChanged( anyString(), anyString(), any(JsonElement.class));
        verify( recordNameChangedListener, times( 0) ).recordNameChanged( anyString(), any(AnonymousRecord.class));

        Assert.assertEquals( connectionMock.lastSentMessage, null );
    }

    @Test
    public void requestsARecordWhenSetNameIsCalled() {
        worksBeforeSetNameIsCalled();

        anonymousRecord.setName( firstRecordName );

        verify( recordNameChangedListener, times( 1) ).recordNameChanged( firstRecordName, anonymousRecord);
        Assert.assertEquals( anonymousRecord.name, firstRecordName );
        Assert.assertEquals( connectionMock.lastSentMessage, TestUtil.replaceSeperators( "R|CR|firstRecordName+" ) );
    }

    @Test
    public void updatesSubscriptionsOnceTheRecordIsReady() {
        requestsARecordWhenSetNameIsCalled();

        recordHandler.handle( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|R|firstRecordName|1|{\"firstname\":\"Wolfram\"}" ), deepstreamClientMock ) );

        //verify( recordEventsListener, times(1) ).onRecordReady( any(Record.class)); //TODO: Should be AnonymousRecord
        verify( recordChangedCallback, times(1) ).onRecordChanged( firstRecordName, gson.fromJson( "{\"firstname\":\"Wolfram\"}", JsonElement.class ) );
        verify( recordChangedCallback, times(1) ).onRecordChanged( firstRecordName, "firstname", new JsonPrimitive("Wolfram") );
    }

    @Test
    public void doesntDoAnythingWhenAnotherRecordChanges() throws DeepstreamRecordDestroyedException {
        updatesSubscriptionsOnceTheRecordIsReady();

        Record secondRecord = recordHandler.getRecord( secondRecordName );
        recordHandler.handle( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|R|secondRecordName|2|{\"firstname\":\"Egon\",\"lastname\":\"Kowalski\"}" ), deepstreamClientMock ) );

        //verify( recordEventsListener, times(1) ).onRecordReady( any(Record.class)); //TODO: Should be AnonymousRecord
        verify( recordChangedCallback, times(1) ).onRecordChanged( firstRecordName, gson.fromJson( "{\"firstname\":\"Wolfram\"}", JsonElement.class ) );
        verify( recordChangedCallback, times(1) ).onRecordChanged( firstRecordName, "firstname", new JsonPrimitive("Wolfram") );
    }

    @Test
    public void movesSubscriptionsToOtherRecordWhenSetNameIsCalled() throws DeepstreamRecordDestroyedException {
        doesntDoAnythingWhenAnotherRecordChanges();
        reset(recordEventsListener);
        reset(recordChangedCallback);

        anonymousRecord.setName( secondRecordName );

        //verify( recordEventsListener, times(2) ).onRecordReady( any(Record.class)); //TODO: Should be AnonymousRecord
        verify( recordChangedCallback, times( 1) ).onRecordChanged( secondRecordName, gson.fromJson( "{\"firstname\":\"Egon\",\"lastname\":\"Kowalski\"}", JsonElement.class ) );
        verify( recordChangedCallback, times(1) ).onRecordChanged(  secondRecordName, "firstname", new JsonPrimitive("Egon") );

        //recordHandler.handle( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|A|D|firstRecordName" ), deepstreamClientMock ) );
        //verify(recordEventsListener, times(1) ).onRecordDiscarded( "recordA" );
    }

    @Test
    public void proxiesCallsThroughToTheUnderlyingRecord() throws DeepstreamRecordDestroyedException {
        movesSubscriptionsToOtherRecordWhenSetNameIsCalled();

        Record secondRecord = recordHandler.getRecord( secondRecordName );

        Assert.assertEquals( secondRecord.get( "lastname" ), new JsonPrimitive("Kowalski") );

        try {
            anonymousRecord.set( "lastname", "Schrader" );
        } catch (AnonymousRecordUninitialized anonymousRecordUninitialized) {
            anonymousRecordUninitialized.printStackTrace();
        }

        Assert.assertEquals( secondRecord.get( "lastname" ),  new JsonPrimitive("Schrader") );
    }

    //TODO
    @Test
    public void notifiedNameChangedWhenSetNameIsCalled() throws DeepstreamRecordDestroyedException {
        proxiesCallsThroughToTheUnderlyingRecord();

        anonymousRecord.setName( thirdRecordName );

        verify( recordNameChangedListener, times(1) ).recordNameChanged( thirdRecordName, anonymousRecord );
        //verify( recordEventsListener, times(1) ).onRecordReady( any( Record.class ) ); //TODO
    }

    @Test
    public void emitsAnAdditonalReadyEventOnceTheNewRecordBecomesAvailable() throws DeepstreamRecordDestroyedException {
        notifiedNameChangedWhenSetNameIsCalled();

        recordHandler.handle( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|R|thirdRecordName|{\"firstname\":\"Egon\",\"lastname\":\"Kowalski\"}" ), deepstreamClientMock ) );

        //verify( recordEventsListener, times( 3 ) ).onRecordReady( any( Record.class ) ); //TODO
    }

}
