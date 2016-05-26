package io.deepstream;

import com.google.gson.JsonObject;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.utils.Emitter;

import java.util.Map;

class Application implements ConnectionChangeListener, LoginCallback {

    public Application() {
        try {
            JsonObject authData = new JsonObject();
            authData.addProperty( "name", "Yasser" );

            DeepstreamClient ds = new DeepstreamClient( "localhost:6021" );
            ds
                    .addConnectionChangeListener( this )
                    .login( authData, this );

            ds.event.emit( "bob" );
            ds.event.emit( "bob", 22 );
            ds.event.emit( "bob", "Hi" );
            ds.event.emit( "bob", true );
            ds.event.emit( "bob", false );
            ds.event.emit( "bob", null );

            ds.event.subscribe( "bob", new Emitter.Listener() {
                public void call(Object... args) {
                    System.out.println( "Received event bob with arguments: " + args[ 0 ] );
                }
            } );
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
