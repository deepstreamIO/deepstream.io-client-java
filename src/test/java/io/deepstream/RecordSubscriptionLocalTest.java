package io.deepstream;


import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import io.deepstream.constants.ConnectionState;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.mockito.Matchers;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import static org.mockito.Mockito.*;

@RunWith( JUnit4.class )
public class RecordSubscriptionLocalTest {

    private Record record;
    private ConnectionMock connectionMock;
    private DeepstreamClientMock deepstreamClientMock;
    private DeepstreamRuntimeErrorHandler errorCallbackMock;
    private RecordChangedCallback subscriptionCallback;

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
        record.onMessage( MessageParser.parseMessage( TestUtil.replaceSeperators( "R|R|testRecord|0|{}" ), deepstreamClientMock ) );
        Assert.assertEquals( new JsonObject(), record.get() );

        subscriptionCallback = mock( RecordChangedCallback.class );
        record.subscribe( subscriptionCallback );
    }

    @After
    public void tearDown() {

    }

    @Test
    public void subscribesToAPath() throws DeepstreamRecordDestroyedException {
        record.subscribe( "firstname", subscriptionCallback );

        JsonObject object = new JsonObject();
        object.addProperty( "firstname", "Wolfram" );

        record.set( "firstname", "Wolfram" );

        verify( subscriptionCallback, times( 1) ).onRecordChanged( "testRecord", object );
        verify( subscriptionCallback, times( 1) ).onRecordChanged( "testRecord", "firstname", object.get( "firstname" ) );
    }

    @Test
    public void subscribesToADifferentPath() throws DeepstreamRecordDestroyedException {
        record.subscribe( "lastname", subscriptionCallback );

        JsonObject object = new JsonObject();
        object.addProperty( "firstname", "Wolfram" );
        object.addProperty( "lastname", "Hempel" );

        record.set( "firstname", "Wolfram" );
        record.set( "lastname", "Hempel" );

        verify( subscriptionCallback, times( 1) ).onRecordChanged( "testRecord", object );
        verify( subscriptionCallback, times( 1) ).onRecordChanged( "testRecord", "lastname", object.get( "lastname" ) );
    }

    @Test
    public void unsubscribesFromAPath() throws DeepstreamRecordDestroyedException {
        subscribesToAPath();
        reset( subscriptionCallback );

        record.unsubscribe( "firstname", subscriptionCallback );

        record.set( "firstname", "Alex" );

        verify( subscriptionCallback, times( 1) ).onRecordChanged( Matchers.matches("testRecord"),  Matchers.any(JsonElement.class) );
        verify( subscriptionCallback, times( 0) ).onRecordChanged( Matchers.anyString(), Matchers.anyString(), Matchers.any() );
    }

    @Test
    public void subscribesToADeepPath() throws DeepstreamRecordDestroyedException {
        record.subscribe( "addresses[1].street", subscriptionCallback );

        JsonObject address = new JsonObject();
        address.addProperty( "street", "someStreet" );

        JsonArray addresses = new JsonArray();
        addresses.add(JsonNull.INSTANCE);
        addresses.add( address );

        JsonObject object = new JsonObject();
        object.add( "addresses", addresses );

        record.set( "addresses[ 1 ].street", "someStreet" );

        verify( subscriptionCallback, times( 1) ).onRecordChanged( "testRecord", object );
        verify( subscriptionCallback, times( 1) ).onRecordChanged(
                "testRecord",
                "addresses[1].street",
                object.get( "addresses" ).getAsJsonArray().get(1).getAsJsonObject().get("street")
        );
    }

    @Test
    public void callsAllCallbacksWhenWholeRecordIsSet() throws DeepstreamRecordDestroyedException {
        record.subscribe( "firstname", subscriptionCallback );
        record.subscribe( "brother.age", subscriptionCallback );

        JsonObject brother = new JsonObject();
        brother.addProperty( "name", "secret" );
        brother.addProperty( "age", "28" );

        JsonObject data = new JsonObject();
        data.addProperty( "firstname", "Wolfram" );
        data.addProperty( "lastname", "Hempel" );
        data.add( "brother", brother);

        record.set( data );

        verify( subscriptionCallback, times( 1) ).onRecordChanged( "testRecord", data );
        verify( subscriptionCallback, times( 1) ).onRecordChanged(
                "testRecord",
                "brother.age",
                data.get( "brother" ).getAsJsonObject().get( "age" )
        );
    }

}
