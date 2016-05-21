package io.deepstream.message;

import io.socket.emitter.Emitter;
import io.socket.engineio.client.Socket;

import java.net.URISyntaxException;
import java.util.Map;

class EndpointEngineIO implements Endpoint {

    Map options;
    Connection connection;
    Socket socket;

    public EndpointEngineIO(String url, Map options, Connection connection) throws URISyntaxException {
        this.connection = connection;

        this.socket = new Socket( url );
        this.addConnectionListeners();
        this.socket.open();
    }

    private void addConnectionListeners() {
        final Connection connection = this.connection;

        this.socket.on( Socket.EVENT_OPEN, new Emitter.Listener() {
            public void call(Object... args) {
                connection.onOpen();
            }
        } );

        this.socket.on( Socket.EVENT_MESSAGE, new Emitter.Listener() {
            public void call(Object... args) {
                connection.onMessage( (String)args[0] );
            }
        });

        this.socket.on(Socket.EVENT_ERROR, new Emitter.Listener() {
            public void call(Object... args) {
                connection.onError( (Exception)args[ 0 ] );
            }
        });
    }

    @Override
    public void send(String message) {
        this.socket.send( message );
    }
}


