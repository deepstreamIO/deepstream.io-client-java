import io.deepstream.ConnectionChangeListener;
import io.deepstream.DeepstreamClient;
import io.deepstream.LoginCallback;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.socket.emitter.Emitter;
import org.json.JSONObject;

import java.util.Map;

public class Test {
    public static void main(String[] args) {
        new Application();
    }
}

class Application implements ConnectionChangeListener, LoginCallback {

    public Application() {
        try {
            JSONObject authData = new JSONObject( "{\"name\":\"Yasser\"}" );

            DeepstreamClient ds = new DeepstreamClient( "ws://localhost:6020" );
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
                    System.out.println( "Recieved event bob with arguments: " + args[ 0 ] );
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

    public void loginFailed( Event errorEvent, String errorMessage ) {
        System.out.println( "Login failed " + errorEvent );
    }
}