package io.deepstream;

import com.google.gson.JsonObject;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.util.Map;

class Application implements ConnectionStateListener, DeepstreamClient.LoginCallback {

    Application() {

        try {
            JsonObject authData = new JsonObject();
            authData.addProperty( "username", "Wolfram" );

            DeepstreamClient ds = new DeepstreamClient( "localhost:6021" );
            ds
                    .addConnectionChangeListener( this )
                    .setRuntimeErrorHandler(new DeepstreamRuntimeErrorHandler() {
                        @Override
                        public void onException(Topic topic, Event event, String errorMessage) {
                            System.out.println( "Error occured " + topic + " " + event + " " + errorMessage);
                        }
                    });

            ds.login(authData);

            authData = new JsonObject();
            authData.addProperty( "username", "Yasser" );

            DeepstreamClient ds2 = new DeepstreamClient( "localhost:6021" );
            ds2
                    .addConnectionChangeListener( this )
                    .setRuntimeErrorHandler(new DeepstreamRuntimeErrorHandler() {
                        @Override
                        public void onException(Topic topic, Event event, String errorMessage) {
                            System.out.println( "Error occured " + topic + " " + event + " " + errorMessage);
                        }
                    });

            ds2.login(authData);
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
