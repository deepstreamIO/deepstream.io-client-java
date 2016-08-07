package io.deepstream;


import com.google.gson.JsonObject;
import io.deepstream.constants.ConnectionState;
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
public class RecordSetWithObjectDeltaTest {

    private Record record;
    private ConnectionMock connectionMock;
    private DeepstreamClientMock deepstreamClientMock;
    private DeepstreamRuntimeErrorHandler errorCallbackMock;
    private boolean useObjectDeltas = false;

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
        options.put( "objectDeltas", "true" );

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
        object.addProperty( "lastname", "Hempel" );
        JsonObject favouriteCoffee = new JsonObject();
        favouriteCoffee.addProperty( "name", "Flat White" );
        favouriteCoffee.addProperty( "price", 5 );
        object.add( "favouriteCoffee", favouriteCoffee );

        record.set( object );

        Assert.assertEquals( connectionMock.lastSentMessage,
                TestUtil.replaceSeperators( "R|U|testRecord|1|{\"firstname\":\"Wolfram\",\"lastname\":\"Hempel\",\"favouriteCoffee\":{\"name\":\"Flat White\",\"price\":5}}+" ) );
        Assert.assertEquals( object, record.get() );
    }

    @Test
    public void sendsUpdateMessageForPathChange() throws DeepstreamRecordDestroyedException {
        sendsUpdateMessageForEntireRecord();

        record.set( "lastname", "HEMPEL" );

        Assert.assertEquals( connectionMock.lastSentMessage, TestUtil.replaceSeperators( "R|P|testRecord|2|lastname|SHEMPEL+" ) );
        Assert.assertEquals( "HEMPEL", record.get( "lastname" ).getAsString() );
    }

    @Test
    public void deletesValueWhenSendingUndefined() throws DeepstreamRecordDestroyedException {
        //TODO
    }

    @Test
    public void sendsDeltaObjectWhenWholePathNotProvided() {
        sendsUpdateMessageForEntireRecord();

        JsonObject newFavouriteCoffee = new JsonObject();
        newFavouriteCoffee.addProperty( "name", "Latte" );
        newFavouriteCoffee.addProperty( "price", 5 );
        record.set( "favouriteCoffee", newFavouriteCoffee );

        Assert.assertEquals( TestUtil.replaceSeperators( "R|P|testRecord|2|favouriteCoffee.name|SLatte+" ), connectionMock.lastSentMessage );
        Assert.assertEquals( "Latte", record.get( "favouriteCoffee.name" ).getAsString() );
    }
}
