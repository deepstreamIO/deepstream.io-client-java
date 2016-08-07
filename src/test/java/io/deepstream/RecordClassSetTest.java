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
import java.util.Arrays;
import java.util.HashMap;
import java.util.Properties;

import static org.mockito.Mockito.mock;

@RunWith( JUnit4.class )
public class RecordClassSetTest {

    private Record record;
    private ConnectionMock connectionMock;
    private DeepstreamClientMock deepstreamClientMock;
    private DeepstreamRuntimeErrorHandler errorCallbackMock;

    @Before
    public void setUp() throws URISyntaxException, InvalidDeepstreamConfig {
        this.connectionMock = new ConnectionMock();
        this.errorCallbackMock = mock(DeepstreamRuntimeErrorHandler.class);
        this.deepstreamClientMock = new DeepstreamClientMock();
        this.deepstreamClientMock.setRuntimeErrorHandler(errorCallbackMock);
        this.deepstreamClientMock.setConnectionState(ConnectionState.OPEN);

        Properties options = new Properties();
        options.put("subscriptionTimeout", "10");
        options.put("recordDeleteTimeout", "10");
        options.put("recordReadAckTimeout", "10");
        options.put("recordReadTimeout", "20");
        options.put( "objectDeltas", "true" );

        this.record = new Record("testRecord", new HashMap(), connectionMock, new DeepstreamConfig(options), deepstreamClientMock);
        record.onMessage(MessageParser.parseMessage(TestUtil.replaceSeperators("R|A|S|testRecord"), deepstreamClientMock));
        record.onMessage(MessageParser.parseMessage(TestUtil.replaceSeperators("R|R|testRecord|0|{}"), deepstreamClientMock));
        Assert.assertEquals(new JsonObject(), record.get());
    }

    @After
    public void tearDown() {

    }

    @Test
    public void sendsUpdateMessageForEntireRecord() throws DeepstreamRecordDestroyedException {
        Person p = new Person("Homer", 30, new String[] {"beer", "food"});
        record.set(p);

        Assert.assertEquals(connectionMock.lastSentMessage, TestUtil.replaceSeperators("R|U|testRecord|1|{\"name\":\"Homer\",\"age\":30,\"likes\":[\"beer\",\"food\"]}+"));
        Assert.assertEquals(p, record.get(Person.class));
    }

    @Test
    public void sendsUpdateMessageForPathChange() throws DeepstreamRecordDestroyedException {
        sendsUpdateMessageForEntireRecord();

        Person p = record.get(Person.class);
        p.name = "Marge";
        record.set(p);

        Assert.assertEquals(connectionMock.lastSentMessage, TestUtil.replaceSeperators("R|P|testRecord|2|name|SMarge+"));
        Assert.assertEquals("Marge", record.get("name").getAsString());
    }

    @Test
    public void sendsUpdateForNestedObject() {
        Movie movie = new Movie("The Shining", 1980, new Person("Stanley K.", -1, null));
        record.set(movie);

        Movie updatedMovie = record.get(Movie.class);
        updatedMovie.director.name = "Stanley Kubrick";
        record.set(updatedMovie);

        Assert.assertEquals(connectionMock.lastSentMessage, TestUtil.replaceSeperators("R|P|testRecord|2|director.name|SStanley Kubrick+"));
    }

    @Test
    // setting a value to null will cause it to be removed from
    // the JsonObject. Will therefore send whole object
    // todo: look into sending delete when field is set to null
    public void deletesValueWhenSendingUndefined() throws DeepstreamRecordDestroyedException {
        sendsUpdateMessageForEntireRecord();

        Person p = record.get(Person.class);
        p.name = null;
        record.set(p);

        Assert.assertEquals(connectionMock.lastSentMessage, TestUtil.replaceSeperators("R|U|testRecord|2|{\"age\":30,\"likes\":[\"beer\",\"food\"]}+"));
        Person updatedPerson = record.get(Person.class);
        Assert.assertEquals(null, updatedPerson.name);
    }

    private class Person {
        String name;
        int age;
        String[] likes;

        public Person(String name, int age, String[] likes) {
            this.name = name;
            this.age = age;
            this.likes = likes;
        }

        @Override
        public boolean equals(Object obj) {
            if ( this == obj ) return true;
            if ( obj == null ) return false;
            if ( this.getClass() != obj.getClass() ) return false;
            Person person = (Person) obj;

            if( !this.name.equals( person.name ) ) return false;
            if( this.age != person.age ) return false;
            if( !Arrays.equals(this.likes, person.likes) ) return false;

            return true;
        }
    }


    private class Movie {
        String name;
        int yearReleased;
        Person director;

        public Movie(String name, int yearReleased, Person director) {
            this.name = name;
            this.yearReleased = yearReleased;
            this.director = director;
        }
    }
}