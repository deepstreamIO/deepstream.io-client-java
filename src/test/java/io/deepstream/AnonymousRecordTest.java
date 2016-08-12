package io.deepstream;


import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Properties;

import static org.mockito.Mockito.*;

public class AnonymousRecordTest {

    Gson gson = new Gson();
    DeepstreamClientMock deepstreamClientMock;
    ConnectionMock connectionMock;
    RecordHandler recordHandler;
    DeepstreamRuntimeErrorHandler errorCallbackMock;
    AnonymousRecord anonymousRecord;
    RecordEventsListener recordEventsListener;
    AnonymousRecordNameChangedListener recordNameChangedListener;
    RecordChangedCallback recordChangedCallback;
    RecordPathChangedCallback recordPathChangedCallback;
    String firstRecordName = "firstRecordName";
    String secondRecordName = "secondRecordName";
    String thirdRecordName = "thirdRecordName";

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
        recordChangedCallback = mock(RecordChangedCallback.class);
        recordPathChangedCallback = mock(RecordPathChangedCallback.class);
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
        Assert.assertEquals( anonymousRecord.name(), null );

        anonymousRecord.addRecordEventsListener(recordEventsListener);
        anonymousRecord.subscribe( recordChangedCallback );
        anonymousRecord.subscribe( "firstname", recordPathChangedCallback );

        verify( recordEventsListener, times( 0 )).onRecordDeleted( anyString() );
        verify( recordEventsListener, times( 0 )).onRecordDiscarded( anyString() );
        verify( recordChangedCallback, times( 0) ).onRecordChanged( anyString(), any(JsonElement.class));
        verify( recordPathChangedCallback, times( 0) ).onRecordPathChanged( anyString(), anyString(), any(JsonElement.class));
        verify( recordNameChangedListener, times( 0) ).recordNameChanged( anyString(), any(AnonymousRecord.class));

        Assert.assertEquals( connectionMock.lastSentMessage, null );
    }

    @Test
    public void requestsARecordWhenSetNameIsCalled() throws InterruptedException {
        worksBeforeSetNameIsCalled();

        new Thread(new Runnable() {
            @Override
            public void run() {
                anonymousRecord.setName( firstRecordName );
            }
        }).start();

        try {
            Thread.sleep(50);
            recordHandler.handle( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|R|firstRecordName|1|{\"firstname\":\"Wolfram\"}" ), deepstreamClientMock ) );
            Thread.sleep(50);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        verify( recordNameChangedListener, times( 1 ) ).recordNameChanged( firstRecordName, anonymousRecord);
        Assert.assertEquals( anonymousRecord.name(), firstRecordName );
        Assert.assertEquals( connectionMock.lastSentMessage, TestUtil.replaceSeperators( "R|CR|firstRecordName+" ) );

        verify( recordChangedCallback, times(1) ).onRecordChanged( firstRecordName, gson.fromJson( "{\"firstname\":\"Wolfram\"}", JsonElement.class ) );
        verify( recordPathChangedCallback, times(1) ).onRecordPathChanged( firstRecordName, "firstname", new JsonPrimitive("Wolfram") );
    }

    @Test
    public void updatesSubscriptionsOnceTheRecordIsReady() throws InterruptedException {
        requestsARecordWhenSetNameIsCalled();
    }

    @Test
    public void doesntDoAnythingWhenAnotherRecordChanges() throws DeepstreamRecordDestroyedException, InterruptedException {
        updatesSubscriptionsOnceTheRecordIsReady();

        new Thread(new Runnable() {
            @Override
            public void run() {
                Record secondRecord = recordHandler.getRecord( secondRecordName );
            }
        }).start();

        try {
            Thread.sleep(50);
            recordHandler.handle( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|R|secondRecordName|2|{\"firstname\":\"Egon\",\"lastname\":\"Kowalski\"}" ), deepstreamClientMock ) );
            Thread.sleep(50);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        verify( recordChangedCallback, times(1) ).onRecordChanged( firstRecordName, gson.fromJson( "{\"firstname\":\"Wolfram\"}", JsonElement.class ) );
        verify( recordPathChangedCallback, times(1) ).onRecordPathChanged( firstRecordName, "firstname", new JsonPrimitive("Wolfram") );
    }

    @Test
    public void movesSubscriptionsToOtherRecordWhenSetNameIsCalled() throws DeepstreamRecordDestroyedException, InterruptedException {
        doesntDoAnythingWhenAnotherRecordChanges();
        resetMocks();

        anonymousRecord.setName( secondRecordName );

        verify( recordChangedCallback, times( 1) ).onRecordChanged( secondRecordName, gson.fromJson( "{\"firstname\":\"Egon\",\"lastname\":\"Kowalski\"}", JsonElement.class ) );
        verify( recordPathChangedCallback, times(1) ).onRecordPathChanged(  secondRecordName, "firstname", new JsonPrimitive("Egon") );

        //TODO
        //recordHandler.handle( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|A|D|firstRecordName" ), deepstreamClientMock ) );
        //verify(recordEventsListener, times(1) ).onRecordDiscarded( "recordA" );
    }

    @Test
    public void proxiesCallsThroughToTheUnderlyingRecord() throws DeepstreamRecordDestroyedException, InterruptedException {
        movesSubscriptionsToOtherRecordWhenSetNameIsCalled();
        resetMocks();

        Record secondRecord = recordHandler.getRecord( secondRecordName );

        Assert.assertEquals( secondRecord.get( "lastname" ), new JsonPrimitive("Kowalski") );

        try {
            anonymousRecord.set( "lastname", "Schrader" );
        } catch (AnonymousRecordUninitialized anonymousRecordUninitialized) {
            anonymousRecordUninitialized.printStackTrace();
        }

        Assert.assertEquals( secondRecord.get( "lastname" ),  new JsonPrimitive("Schrader") );
    }

    @Test
    public void notifiedNameChangedWhenSetNameIsCalled() throws DeepstreamRecordDestroyedException, InterruptedException {
        proxiesCallsThroughToTheUnderlyingRecord();
        resetMocks();

        new Thread(new Runnable() {
            @Override
            public void run() {
                anonymousRecord.setName( thirdRecordName );
            }
        }).start();

        try {
            Thread.sleep(50);
            recordHandler.handle( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|R|thirdRecordName|1|{\"firstname\":\"Wolfram\"}" ), deepstreamClientMock ) );
            Thread.sleep(50);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        verify( recordNameChangedListener, times(1) ).recordNameChanged( thirdRecordName, anonymousRecord );
    }

    private void resetMocks() {
        reset(recordEventsListener);
        reset(recordChangedCallback);
    }

}
