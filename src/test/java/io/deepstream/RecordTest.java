package io.deepstream;


import com.google.gson.JsonObject;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.HashMap;
import java.util.Properties;

import static org.mockito.Mockito.*;

public class RecordTest {
    DeepstreamClientMock deepstreamClientMock;
    ConnectionMock connectionMock;
    RecordHandler recordHandler;
    DeepstreamRuntimeErrorHandler errorCallbackMock;
    Record record;
    RecordEventsListener recordEventsListener;
    Record.RecordReadyListener recordReadyListener;
    DeepstreamConfig config;

    @Before
    public void setUp() throws InvalidDeepstreamConfig, InterruptedException {

        this.connectionMock = new ConnectionMock();
        this.errorCallbackMock = mock( DeepstreamRuntimeErrorHandler.class );
        this.deepstreamClientMock = new DeepstreamClientMock();
        this.deepstreamClientMock.setRuntimeErrorHandler( errorCallbackMock );
        this.deepstreamClientMock.setConnectionState( ConnectionState.OPEN );

        Properties options = new Properties();
        options.put( "subscriptionTimeout", "50" );
        options.put( "recordDeleteTimeout", "50" );
        options.put( "recordReadAckTimeout", "50" );
        options.put( "recordReadTimeout", "200" );
        config = new DeepstreamConfig( options );

        recordHandler = new RecordHandler( config, connectionMock, deepstreamClientMock );
        recordEventsListener = mock(RecordEventsListener.class);
        recordReadyListener = mock(Record.RecordReadyListener.class);

        Thread b = new Thread(new Runnable() {
            @Override
            public void run() {
                record = new Record( "recordA", new HashMap(), connectionMock, config, deepstreamClientMock );
                record.addRecordEventsListener(recordEventsListener);
                record.start();
            }
        });
        b.start();

        while( record == null ) {
            Thread.sleep( 10 );
        }
    }

    @After
    public void tearDown() {
    }

    @Test
    public void recordSendsAckAndRead() {
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|A|S|recordA" ), deepstreamClientMock ) );
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|R|recordA|0|{ \"name\": \"sam\" }" ), deepstreamClientMock ) );
    }

    @Test
    public void recordHasSendCreateReadMessage() {
        recordSendsAckAndRead();
        Assert.assertEquals( connectionMock.lastSentMessage, TestUtil.replaceSeperators( "R|CR|recordA+" ) );
    }

    @Test
    public void recordHasCorrectDefaultState() {
        recordSendsAckAndRead();
        Assert.assertFalse( record.isDestroyed() );
    }

    @Test
    public void recordReturnsObjectCorrectly() {
        recordSendsAckAndRead();
        JsonObject data = new JsonObject();
        data.addProperty( "name", "sam" );

        Assert.assertEquals( data, record.get() );
    }

/*    @Test
    public void recordReturnsObjectPathsCorrectly() {
        JsonObject data = new JsonObject();
        data.addProperty( "name", "sam" );

        recordInitialisedCorrectly();
        Assert.assertEquals( "sam", record.get( "name" ) );
    }*/

    @Test
    public void recordDiscardsCorrectly() throws DeepstreamRecordDestroyedException {
        recordSendsAckAndRead();

        record.discard();

        Assert.assertEquals( connectionMock.lastSentMessage, TestUtil.replaceSeperators( "R|US|recordA+" ) );
        Assert.assertTrue( record.isReady() );
        Assert.assertFalse( record.isDestroyed() );
    }

    @Test
    public void emitsDiscardEventOnDiscardAck() throws DeepstreamRecordDestroyedException {
        recordSendsAckAndRead();

        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|A|US|recordA" ), deepstreamClientMock ) );

        Assert.assertFalse( record.isReady() );
        Assert.assertTrue( record.isDestroyed() );

        verify(recordEventsListener, times(1) ).onRecordDiscarded( "recordA" );
    }

    @Test
    public void recordDeletesCorrectly() throws DeepstreamRecordDestroyedException {
        recordSendsAckAndRead();

        record.delete();

        Assert.assertEquals( connectionMock.lastSentMessage, TestUtil.replaceSeperators( "R|D|recordA+" ) );
        Assert.assertTrue( record.isReady() );
        Assert.assertFalse( record.isDestroyed() );
    }

    @Test
    public void emitsDeleteEventOnDeleteAck() throws DeepstreamRecordDestroyedException {
        recordDeletesCorrectly();
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|A|D|recordA" ), deepstreamClientMock ) );

        Assert.assertFalse( record.isReady() );
        Assert.assertTrue( record.isDestroyed() );

        verify(recordEventsListener, times(1) ).onRecordDeleted( "recordA" );
    }

    @Test
    public void unsolicitatedDeleteAckMessages() throws DeepstreamRecordDestroyedException {
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|A|D|recordA" ), deepstreamClientMock ) );
        verify( errorCallbackMock, times( 1 ) ).onException(Topic.RECORD, Event.UNSOLICITED_MESSAGE, TestUtil.replaceSeperators( "R|A|D|recordA" ) );
    }

    @Test
    public void unsolicitatedDiscardAckMessages() throws DeepstreamRecordDestroyedException {
        recordSendsAckAndRead();

        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|A|US|recordA" ), deepstreamClientMock ) );
        verify( errorCallbackMock, times( 1 ) ).onException(Topic.RECORD, Event.UNSOLICITED_MESSAGE, TestUtil.replaceSeperators( "R|A|US|recordA" ) );
    }

    @Test
    public void subscribeTimeout() throws DeepstreamRecordDestroyedException, InterruptedException {
        Thread.sleep( 50 );
        verify( errorCallbackMock, times( 1 ) ).onException(Topic.RECORD, Event.ACK_TIMEOUT, "No ACK message received in time for SUBSCRIBE recordA" );
    }

    @Test
    public void readTimeout() throws DeepstreamRecordDestroyedException, InterruptedException {
        try {
            Thread.sleep(200);
            record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|A|S|recordA" ), deepstreamClientMock ) );
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        Thread.sleep( 100 );
        verify( errorCallbackMock, times( 1 ) ).onException(Topic.RECORD, Event.RESPONSE_TIMEOUT, "No message received in time for READ recordA" );
    }

    @Test
    public void discardTimeout() throws DeepstreamRecordDestroyedException, InterruptedException {
        recordDiscardsCorrectly();

        Thread.sleep( 100 );
        verify( errorCallbackMock, times( 1 ) ).onException(Topic.RECORD, Event.ACK_TIMEOUT, "No ACK message received in time for UNSUBSCRIBE recordA" );
    }

    @Test
    public void deleteTimout() throws DeepstreamRecordDestroyedException, InterruptedException {
        recordDeletesCorrectly();

        Thread.sleep( 100 );
        verify( errorCallbackMock, times( 1 ) ).onException(Topic.RECORD, Event.DELETE_TIMEOUT, "No message received in time for DELETE recordA" );
    }

}
