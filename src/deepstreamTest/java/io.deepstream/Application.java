package io.deepstream;

import com.google.gson.JsonObject;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.util.Map;

class Application implements ConnectionChangeListener, LoginCallback {

    public Application() {

        try {
            JsonObject authData = new JsonObject();
            authData.addProperty( "username", "Wolfram" );

            DeepstreamClient ds = new DeepstreamClient( "localhost:6021" );
            ds
                    .addConnectionChangeListener( this )
                    .setRuntimeErrorHandler(new DeepstreamRuntimeErrorHandler() {
                        @Override
                        public void onException(Topic topic, Event event, String msg) {
                            System.out.println( "Error occured " + topic + " " + event + " " + msg );
                        }
                    })
                    .login( authData, this );

            Thread.sleep(1000);
            authData = new JsonObject();
            authData.addProperty( "username", "Yasser" );

            DeepstreamClient ds2 = new DeepstreamClient( "localhost:6021" );
            ds2
                    .addConnectionChangeListener( this )
                    .setRuntimeErrorHandler(new DeepstreamRuntimeErrorHandler() {
                        @Override
                        public void onException(Topic topic, Event event, String msg) {
                            System.out.println( "Error occured " + topic + " " + event + " " + msg );
                        }
                    })
                    .login( authData, this );
        }

        catch( Exception e ) {
            System.out.println( e );
        }

    }

    public void connectionStateChanged( ConnectionState connectionState ) {
        System.out.println( "Connection state changed " +  connectionState );
    }

    public void loginSuccess( Map loginData ) {
        System.out.println( "Login Success" );
    }

    public void loginFailed(Event errorEvent, Object errorMessage ) {
        System.out.println( "Login failed " + errorEvent.toString() );
    }
}
