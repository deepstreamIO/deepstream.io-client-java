package io.deepstream;

import com.google.gson.JsonObject;
import io.deepstream.constants.ConfigOptions;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.util.Map;
import java.util.Properties;

class Application implements ConnectionStateListener, DeepstreamClient.LoginCallback {

    Application() throws InvalidDeepstreamConfig {

        try {
            JsonObject authData = new JsonObject();
            authData.addProperty("username", "Yasser");

            Properties config = new Properties();
            config.put(ConfigOptions.SUBSCRIPTION_TIMEOUT.toString(), 100000);
            config.put(ConfigOptions.RECORD_READ_ACK_TIMEOUT.toString(), 100000);
            config.put(ConfigOptions.RECORD_READ_TIMEOUT.toString(), 100000);

            DeepstreamClient ds = new DeepstreamClient("localhost:6021", config);
            ds
                    .addConnectionChangeListener( this )
                    .setRuntimeErrorHandler(new DeepstreamRuntimeErrorHandler() {
                        @Override
                        public void onException(Topic topic, Event event, String errorMessage) {
                            System.out.println( "Error occured " + topic + " " + event + " " + errorMessage);
                        }
                    });

            ds.login(authData);

            ds.event.subscribe("event-a", new EventListener() {
                @Override
                public void onEvent(String eventName, Object... args) {

                }
            });


            Record record = ds.record.getRecord(ds.getUid());
            JsonObject data = new JsonObject();
            data.addProperty("color", ds.getUid());
            record.set(data);
            System.out.println("record " + record.get());

            List list = ds.record.getList("a-list");
            list.addEntry(ds.getUid());
            System.out.println("list " + list.getEntries());

            AnonymousRecord anonymousRecord = ds.record.getAnonymousRecord();
            anonymousRecord.setName(list.getEntries().get(0));
            System.out.println("anonymousRecord " + anonymousRecord.get());
        }

        catch( Exception e ) {
           e.printStackTrace();
        }

    }


    public void loginSuccess( Map userData) {
        System.out.println( "Login Success" );
    }

    @Override
    public void loginFailed(Event errorEvent, Object data) {
        System.out.println( "Login failed " + errorEvent.toString() );
    }

    @Override
    public void connectionStateChanged(ConnectionState connectionState) {
        System.out.println( "Connection state changed " +  connectionState );
    }
}
