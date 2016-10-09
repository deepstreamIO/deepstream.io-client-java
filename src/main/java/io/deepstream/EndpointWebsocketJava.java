package io.deepstream;

import org.java_websocket.client.WebSocketClient;
import org.java_websocket.drafts.Draft;
import org.java_websocket.drafts.Draft_10;
import org.java_websocket.drafts.Draft_75;
import org.java_websocket.handshake.ServerHandshake;

import java.net.URI;
import java.net.URISyntaxException;

class EndpointWebsocketJava implements Endpoint {

    private final URI originalURI;
    private WebSocket websocket;
    private final Connection connection;

    public EndpointWebsocketJava(String url, DeepstreamConfig deepstreamConfig, Connection connection ) throws URISyntaxException {
        try {
            originalURI = URI.create(url);
        } catch( Exception e ) {
            throw new URISyntaxException( url, "URL provided is not correct" );
        }

        this.connection = connection;
        websocket = new WebSocket( originalURI, new Draft_10() );
    }

    @Override
    public void send(String message) {
        System.out.println( "Sending ");
        websocket.send( message );
    }

    @Override
    public void close() {
        websocket.close();
    }

    @Override
    public void open() {
        websocket.connect();
    }

    class WebSocket extends WebSocketClient {
        public WebSocket( URI serverUri , Draft draft  ) {
            super( serverUri, draft );
            System.out.println( serverUri );
        }

        @Override
        public void onOpen(ServerHandshake handshakedata) {
            connection.onOpen();
        }

        @Override
        public void onMessage(String message) {
            System.out.println( message );
            connection.onMessage( message );
        }

        @Override
        public void onClose(int code, String reason, boolean remote) {
            try {
                connection.onClose();
            } catch( Exception e ) {
            }
        }

        @Override
        public void onError(Exception ex) {
            connection.onError( ex.getMessage() );
        }
    }
}