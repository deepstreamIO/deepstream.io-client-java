package io.deepstream;


import com.google.gson.JsonElement;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.mockito.Matchers;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@RunWith( JUnit4.class )
public class RecordSubscriptionRemoteTest {

    private Record record;
    private ConnectionMock connectionMock;
    private DeepstreamClientMock deepstreamClientMock;
    private DeepstreamRuntimeErrorHandler errorCallbackMock;
    private RecordChangedCallback subscriptionCallback;
    private RecordEventsListener recordEventsListener;

    @Before
    public void setUp() throws DeepstreamRecordDestroyedException {
        this.connectionMock = new ConnectionMock();
        this.errorCallbackMock = mock( DeepstreamRuntimeErrorHandler.class );
        this.deepstreamClientMock = new DeepstreamClientMock();
        this.deepstreamClientMock.setRuntimeErrorHandler( errorCallbackMock );
        this.deepstreamClientMock.setConnectionState( ConnectionState.OPEN );

        Map options = new Properties();
        options.put( "subscriptionTimeout", "10" );
        options.put( "recordDeleteTimeout", "10" );
        options.put( "recordReadAckTimeout", "10" );
        options.put( "recordReadTimeout", "20" );

        this.record = new Record( "testRecord", new HashMap(), connectionMock, options, deepstreamClientMock );
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|A|S|testRecord" ), deepstreamClientMock ) );

        subscriptionCallback = mock( RecordChangedCallback.class );
        record.subscribe( subscriptionCallback );
    }

    @After
    public void tearDown() {

    }

    @Test
    public void callsSubscriptionOnFirstReady() throws DeepstreamRecordDestroyedException {

        record.addRecordEventsListener(new RecordEventsListener() {
            @Override
            public void onError(String recordName, Event errorType, String errorMessage) {

            }

            @Override
            public void onDestroyPending(String recordName) {

            }

            @Override
            public void onRecordDeleted(String recordName) {

            }

            @Override
            public void onRecordDiscarded(String recordName) {

            }

            @Override
            public void onRecordReady(Record record) {
                record.subscribe( subscriptionCallback );
            }
        });

        verify( subscriptionCallback, times(0) ).onRecordChanged(Matchers.anyString(), Matchers.anyString(), Matchers.any() );
        verify( subscriptionCallback, times(0) ).onRecordChanged(Matchers.anyString(), Matchers.any(JsonElement.class) );

        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|R|testRecord|0|{ \"firstName\": \"oldName\" }" ), deepstreamClientMock ) );

        verify( subscriptionCallback, times(0) ).onRecordChanged(Matchers.anyString(), Matchers.anyString(), Matchers.any() );
        verify( subscriptionCallback, times(1) ).onRecordChanged(Matchers.anyString(), Matchers.any(JsonElement.class) );
    }

}
