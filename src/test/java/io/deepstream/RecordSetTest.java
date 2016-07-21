package io.deepstream;


import io.deepstream.constants.Actions;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Topic;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import static org.mockito.Mockito.mock;
@RunWith( JUnit4.class )
public class RecordSetTest {

    private Record record;
    private ConnectionMock connectionMock;
    private DeepstreamClientMock deepstreamClientMock;
    private Person personMock;
    private DeepstreamRuntimeErrorHandler errorCallbackMock;

    @Before
    public void setUp() throws URISyntaxException {
        this.personMock = new Person( "Fred", "Weasley" );

        this.connectionMock = new ConnectionMock();
        this.errorCallbackMock = mock( DeepstreamRuntimeErrorHandler.class );
        this.deepstreamClientMock = new DeepstreamClientMock();
        this.deepstreamClientMock.setRuntimeErrorHandler( errorCallbackMock );
        this.deepstreamClientMock.setConnectionState( ConnectionState.OPEN );

        Map options = new Properties();
        options.put( "recordReadAckTimeout", "10" );
        options.put( "recordReadTimeout", "10" );
        this.record = new Record( "testRecord", new HashMap(), connectionMock, options, deepstreamClientMock );
    }

    @After
    public void tearDown() {

    }

    /*
        Typed record tests
     */
    @Test
    public void createsTheRecord() {
        record.onMessage( new Message(
                "raw",
                Topic.RECORD,
                Actions.READ,
                new String[] { "testRecord", String.valueOf( 0 ), "{\"firstName\":\"Fred\",\"lastName\":\"Weasley\"}" }
        ));
        Person person = record.get( Person.class );
        Assert.assertTrue( person.equals( personMock )  );
        Assert.assertEquals( TestUtil.replaceSeperators("R|CR|testRecord+"), connectionMock.lastSentMessage );
    }

    @Test
    public void sendsUpdateForEntireDataChange() throws DeepstreamRecordDestroyedException {
        record.onMessage( new Message(
                "raw",
                Topic.RECORD,
                Actions.READ,
                new String[] { "testRecord", String.valueOf( 0 ), "{\"firstName\":\"Fred\",\"lastName\":\"Weasley\"}" }
        ));
        record.set( new Person( "Harry" ) );
        Assert.assertEquals( TestUtil.replaceSeperators("R|U|testRecord|1|{\"firstName\":\"Harry\"}+"), connectionMock.lastSentMessage );
    }

    @Test
    public void sendsPatchForSinglePathUpdate() throws DeepstreamRecordDestroyedException {
        record.onMessage( new Message(
                "raw",
                Topic.RECORD,
                Actions.READ,
                new String[] { "testRecord", String.valueOf( 0 ), "{\"firstName\":\"Fred\",\"lastName\":\"Weasley\"}" }
        ));
        Person p = record.get( Person.class );
        p.firstName = "George";
        record.set( p );
        Assert.assertEquals( TestUtil.replaceSeperators("R|P|testRecord|1|firstName|SGeorge+"), connectionMock.lastSentMessage );
    }

    /*it( 'deletes value when sending undefined', function(){
        record.set( 'lastname', undefined );
        expect( connection.lastSendMessage ).toBe( msg( 'R|P|testRecord|3|lastname|U+' ) );
        expect( record.get() ).toEqual( { firstname: 'Wolfram' } );
    });

    it( 'throws error for invalid record data', function(){
        expect(function(){ record.set( undefined ); }).toThrow();
    });*/


    class Person {

        String firstName;
        String lastName;

        Person( String firstName ) {
            this.firstName = firstName;
        }

        Person( String firstName, String lastName ) {
            this.firstName = firstName;
            this.lastName = lastName;
        }

        @Override
        public boolean equals(Object obj) {
            if( !( obj instanceof Person ) ) {
                return false;
            }

            Person other = (Person) obj;
            if( this.firstName.equals( other.firstName) && this.lastName.equals( other.lastName ) )
                return true;
            else
                return false;
        }
    }
}
