package io.deepstream;


import com.google.gson.JsonArray;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Properties;

import static org.mockito.Mockito.mock;

@RunWith( JUnit4.class )
public class RecordSetTest {

    private Record record;
    private ConnectionMock connectionMock;
    private DeepstreamClientMock deepstreamClientMock;
    private DeepstreamRuntimeErrorHandler errorCallbackMock;

    @Before
    public void setUp() throws URISyntaxException, InvalidDeepstreamConfig {
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

        this.record = new Record( "testRecord", new HashMap(), connectionMock, new DeepstreamConfig( options ), deepstreamClientMock );
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|A|S|testRecord" ), deepstreamClientMock ) );
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|R|testRecord|0|{}" ), deepstreamClientMock ) );
        Assert.assertEquals( new JsonObject(), record.get() );
    }

    @After
    public void tearDown() {

    }

    @Test
    public void sendsUpdateMessageForEntireRecord() throws DeepstreamRecordDestroyedException {
        JsonObject object = new JsonObject();
        object.addProperty( "firstname", "Wolfram" );
        record.set( object );

        Assert.assertEquals( connectionMock.lastSentMessage, TestUtil.replaceSeperators( "R|U|testRecord|1|{\"firstname\":\"Wolfram\"}+" ) );
        Assert.assertEquals( object, record.get() );
    }

    @Test
    public void sendsUpdateMessageForPathChange() throws DeepstreamRecordDestroyedException {
        sendsUpdateMessageForEntireRecord();

        record.set( "lastname", "Hempel" );

        Assert.assertEquals( connectionMock.lastSentMessage, TestUtil.replaceSeperators( "R|P|testRecord|2|lastname|SHempel+" ) );
        Assert.assertEquals( "Hempel", record.get( "lastname" ).getAsString() );
    }

    @Test
    public void sendsPatchForNestedUpdate() {
        record.set( "address[0].street", "randomStreet" );
        Assert.assertEquals( connectionMock.lastSentMessage, TestUtil.replaceSeperators( "R|P|testRecord|1|address[0].street|SrandomStreet+" ) );
        Assert.assertEquals( record.get( "address[ 0 ].street" ).getAsString(), "randomStreet" );
    }

    @Test
    public void deletesValueWhenReceivingUndefined() {
        record.set( "firstname", "Alex" );
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|P|testRecord|2|firstname|U+" ), deepstreamClientMock ) );
        Object o = record.get("firstname");
        Assert.assertTrue( record.get( "firstname" ).isJsonNull() );
    }

    @Test
    public void deletesNestedObjectValueWhenReceivingUndefined() {
        record.set( "firstname", "Alex" );
        JsonObject drinks = new JsonObject();
        drinks.addProperty( "beer", "pilsner" );
        drinks.addProperty( "coffee", "flat white" );
        record.set( "drinks", drinks );
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|P|testRecord|3|drinks.beer|U+" ), deepstreamClientMock ) );
        Assert.assertTrue( record.get( "drinks.beer" ).isJsonNull() );
    }

    @Test
    public void deletesArrayValueWhenReceivingUndefined() {
        record.set( "firstname", "Alex" );
        JsonArray drinks = new JsonArray();
        drinks.add( "beer" ); drinks.add( "coffee" );
        record.set( "drinks", drinks );
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|P|testRecord|3|drinks[ 0 ]|U+" ), deepstreamClientMock ) );
        Assert.assertEquals( record.get( "drinks[ 0 ]" ).getAsString(), "coffee" );
    }

    @Test
    public void deletesNestedArrayValueWhenReceivingUndefined() {
        record.set( "address[0].street", "Alex" );
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|P|testRecord|2|address[ 0 ].street|U+" ), deepstreamClientMock ) );
        Object o = record.get( "address[ 0 ].street" );
        Assert.assertTrue( record.get( "address[ 0 ].street" ).isJsonNull()  );
    }

    @Test
    public void receivesWriteAcknowledgement() {
        sendsUpdateMessageForEntireRecord();
        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    Thread.sleep(100);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                record.onMessage( new Message(
                        "raw",
                        Topic.RECORD,
                        Actions.WRITE_ACKNOWLEDGEMENT,
                        new String[]{ "testRecord", "[2]", "L" }
                ));
            }
        }).start();
        JsonObject jsonObject = new JsonObject();
        jsonObject.addProperty("newKey", "newValue");
        RecordSetResult res = record.setWithAck( jsonObject );
        Assert.assertNull( res.getResult() );
    }

    @Test
    public void receivesWriteAcknowledgementWithPath() {
        sendsUpdateMessageForEntireRecord();
        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    Thread.sleep(100);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                record.onMessage( new Message(
                        "raw",
                        Topic.RECORD,
                        Actions.WRITE_ACKNOWLEDGEMENT,
                        new String[]{ "testRecord", "[2]", "L" }
                ));
            }
        }).start();
        RecordSetResult res = record.setWithAck( "lastname", "Hempel" );
        Assert.assertNull( res.getResult() );
    }

    @Test
    public void receivesWriteAcknowledgementError() {
        sendsUpdateMessageForEntireRecord();
        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    Thread.sleep(100);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                record.onMessage( new Message(
                        "raw",
                        Topic.RECORD,
                        Actions.WRITE_ACKNOWLEDGEMENT,
                        new String[]{ "testRecord", "[2]", "SStorage write error" }
                ));
            }
        }).start();
        JsonObject jsonObject = new JsonObject();
        jsonObject.addProperty("newKey", "newValue");
        RecordSetResult res = record.setWithAck( jsonObject );
        Assert.assertEquals( res.getResult(), "Storage write error" );
    }

    @Test
    public void receivesWriteAcknowledgementErrorWithPath() {
        sendsUpdateMessageForEntireRecord();
        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    Thread.sleep(100);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                record.onMessage( new Message(
                        "raw",
                        Topic.RECORD,
                        Actions.WRITE_ACKNOWLEDGEMENT,
                        new String[]{ "testRecord", "[2]", "SCache write error" }
                ));
            }
        }).start();
        RecordSetResult res = record.setWithAck( "lastname", "Hempel" );
        Assert.assertEquals( res.getResult(), "Cache write error" );
    }
}
