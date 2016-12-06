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
    public void deletesValueWhenSendingUndefined() throws DeepstreamRecordDestroyedException {
        //TODO
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
    public void deletesNestedArrayValueWhenReceivingUndefined() {
        record.set( "firstname", "Alex" );
        JsonArray drinks = new JsonArray();
        drinks.add( "beer" ); drinks.add( "coffee" );
        record.set( "drinks", drinks );
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|P|testRecord|3|drinks[ 0 ]|U+" ), deepstreamClientMock ) );
        Assert.assertEquals( record.get( "drinks[ 0 ]" ).getAsString(), "coffee" );
    }
}
