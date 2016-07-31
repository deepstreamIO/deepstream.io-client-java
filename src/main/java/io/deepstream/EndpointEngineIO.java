package io.deepstream;

import io.socket.emitter.Emitter;
import io.socket.engineio.client.Socket;

import java.net.URISyntaxException;
import java.util.Map;

/**
 * An implementation of {@link Endpoint} that allows use to interact with an engine.io server
 * for benefits such connection upgrades against the disadvantage of more processing.
 */
class EndpointEngineIO implements Endpoint {

    Map options;
    Connection connection;
    Socket socket;

    /**
     * @param url
     * @param options
     * @param connection
     * @throws URISyntaxException
     */
    public EndpointEngineIO(String url, Map options, Connection connection) throws URISyntaxException {
        this.connection = connection;

        this.socket = new Socket( url );
        this.addConnectionListeners();
        this.socket.open();
    }

    /**
     *
     */
    private void addConnectionListeners() {
        final Connection connection = this.connection;

        this.socket.on( Socket.EVENT_OPEN, new Emitter.Listener() {
            public void call(Object... args) {
                connection.onOpen();
            }
        } );

        this.socket.on( Socket.EVENT_MESSAGE, new Emitter.Listener() {
            public void call(Object... args) {
                try {
                    connection.onMessage( (String)args[0] );
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        });

        /**
         *
         */
        this.socket.on(Socket.EVENT_ERROR, new Emitter.Listener() {
            public void call(Object... args) {
                Exception e = (Exception)args[ 0 ];
                connection.onError( e.getMessage() );
            }
        });
    }

    @Override
    public void send(String message) {
        this.socket.send( message );
    }

    @Override
    public void close() {

    }

    @Override
    public void open() {

    }
}


