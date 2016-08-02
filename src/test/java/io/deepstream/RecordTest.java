package io.deepstream;


import com.google.gson.JsonObject;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import static org.mockito.Mockito.*;

public class RecordTest {

    Map options;
    DeepstreamClientMock deepstreamClientMock;
    ConnectionMock connectionMock;
    RecordHandler recordHandler;
    DeepstreamRuntimeErrorHandler errorCallbackMock;
    Record record;
    RecordEventsListener recordEventsListener;
    RecordReadyListener recordReadyListener;

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
        recordReadyListener = mock(RecordReadyListener.class);
    }

    @After
    public void tearDown() {
    }

    @Test
    public void recordHasCorrectDefaultState() {
        record = new Record( "recordA", new HashMap(), connectionMock, options, deepstreamClientMock );
        record.addRecordEventsListener(recordEventsListener);
        record.addRecordReadyListener( recordReadyListener );
        Assert.assertFalse( record.isReady() );
        Assert.assertFalse( record.isDestroyed() );
    }

    @Test
    public void recordSendsDownCorrectCreateMessage() {
        recordHasCorrectDefaultState();
        Assert.assertEquals( connectionMock.lastSentMessage, TestUtil.replaceSeperators( "R|CR|recordA+" ) );
    }

    @Test
    public void recordInitialisedCorrectly() {
        recordSendsDownCorrectCreateMessage();
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|A|S|recordA" ), deepstreamClientMock ) );
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|R|recordA|0|{ \"name\": \"sam\" }" ), deepstreamClientMock ) );
        Assert.assertTrue( record.isReady() );
        verify( recordReadyListener, times(1) ).onRecordReady( "recordA", record );
    }

    @Test
    public void recordReturnsObjectCorrectly() {
        JsonObject data = new JsonObject();
        data.addProperty( "name", "sam" );

        recordInitialisedCorrectly();
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
        recordInitialisedCorrectly();
        record.discard();

        Assert.assertEquals( connectionMock.lastSentMessage, TestUtil.replaceSeperators( "R|US|recordA+" ) );
        Assert.assertTrue( record.isReady() );
        Assert.assertFalse( record.isDestroyed() );
    }

    @Test
    public void emitsDiscardEventOnDiscardAck() throws DeepstreamRecordDestroyedException {
        recordDiscardsCorrectly();
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|A|US|recordA" ), deepstreamClientMock ) );

        Assert.assertFalse( record.isReady() );
        Assert.assertTrue( record.isDestroyed() );

        verify(recordEventsListener, times(1) ).onRecordDiscarded( "recordA" );
    }

    @Test
    public void recordDeletesCorrectly() throws DeepstreamRecordDestroyedException {
        recordInitialisedCorrectly();
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
        recordInitialisedCorrectly();
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|A|D|recordA" ), deepstreamClientMock ) );
        verify( errorCallbackMock, times( 1 ) ).onException(Topic.RECORD, Event.UNSOLICITED_MESSAGE, TestUtil.replaceSeperators( "R|A|D|recordA" ) );
    }

    @Test
    public void unsolicitatedDiscardAckMessages() throws DeepstreamRecordDestroyedException {
        recordInitialisedCorrectly();
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|A|US|recordA" ), deepstreamClientMock ) );
        verify( errorCallbackMock, times( 1 ) ).onException(Topic.RECORD, Event.UNSOLICITED_MESSAGE, TestUtil.replaceSeperators( "R|A|US|recordA" ) );
    }

    @Test
    public void subscribeTimeout() throws DeepstreamRecordDestroyedException, InterruptedException {
        recordSendsDownCorrectCreateMessage();
        Thread.sleep( 50 );
        verify( errorCallbackMock, times( 1 ) ).onException(Topic.RECORD, Event.ACK_TIMEOUT, "No ACK message received in time for SUBSCRIBE recordA" );
    }

    @Test
    public void readTimeout() throws DeepstreamRecordDestroyedException, InterruptedException {
        recordSendsDownCorrectCreateMessage();
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|A|S|recordA" ), deepstreamClientMock ) );
        Thread.sleep( 50 );
        verify( errorCallbackMock, times( 1 ) ).onException(Topic.RECORD, Event.RESPONSE_TIMEOUT, "No message received in time for READ recordA" );
    }

    @Test
    public void discardTimeout() throws DeepstreamRecordDestroyedException, InterruptedException {
        recordDiscardsCorrectly();
        Thread.sleep( 50 );
        verify( errorCallbackMock, times( 1 ) ).onException(Topic.RECORD, Event.ACK_TIMEOUT, "No ACK message received in time for UNSUBSCRIBE recordA" );
    }

    @Test
    public void deleteTimout() throws DeepstreamRecordDestroyedException, InterruptedException {
        recordDeletesCorrectly();
        Thread.sleep( 50 );
        verify( errorCallbackMock, times( 1 ) ).onException(Topic.RECORD, Event.DELETE_TIMEOUT, "No message received in time for DELETE recordA" );
    }

}
