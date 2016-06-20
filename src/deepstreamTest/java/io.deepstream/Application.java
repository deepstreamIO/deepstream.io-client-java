package io.deepstream;

import com.google.gson.JsonObject;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.rpc.RpcCallback;
import io.deepstream.rpc.RpcResponse;
import io.deepstream.rpc.RpcResponseCallback;
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

            ds.rpc.provide( "rpcName", new RpcCallback() {
                @Override
                public void Call( Object data, RpcResponse response ) {
                    response.send( "Success!" );
                }
            });

            ds.rpc.make("boib", new JsonObject(), new RpcResponseCallback() {
                @Override
                public void onData(Object data) {
                    System.out.println( data );
                }

                @Override
                public void onError(String err) {
                    System.out.println( err );
                }
            });
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
