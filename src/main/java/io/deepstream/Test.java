import io.deepstream.ConnectionChangeListener;
import io.deepstream.DeepstreamClient;
import io.deepstream.LoginCallback;
import io.deepstream.constants.ConnectionState;
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
        }
        catch( Exception e ) {

        }
    }

    public void connectionStateChanged( ConnectionState connectionState ) {
        System.out.println( "Connection state changed " +  connectionState );
    }

    public void loginSuccess( Map loginData ) {
        System.out.println( "Login Success" );
    }

    public void loginFailed( String errorEvent ) {
        System.out.println( "Login failed " + errorEvent );
    }
}