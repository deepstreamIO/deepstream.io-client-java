package io.deepstream;

import org.java_websocket.WebSocket;
import org.java_websocket.WebSocket.READYSTATE;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.drafts.Draft;
import org.java_websocket.drafts.Draft_10;
import org.java_websocket.handshake.ServerHandshake;

import java.net.URI;
import java.net.URISyntaxException;

class EndpointWebsocket implements Endpoint {

    private URI originalURI;
    private WebSocket websocket;
    private final Connection connection;

    public EndpointWebsocket(String url, DeepstreamConfig deepstreamConfig, Connection connection ) throws URISyntaxException {
        originalURI = parseUri( url, deepstreamConfig.getPath() );
        this.connection = connection;

        websocket = new WebSocket( originalURI, new Draft_10() );
        websocket.connect();
    }

    /**
     * Take the url passed when creating the client and ensure the correct
     * protocol is provided
     * @param  {String} url Url passed in by client
     * @return {String} Url with supported protocol
     */
    private URI parseUri(String url, String defaultPath ) throws URISyntaxException {
        if( url.matches( "^http:|^https:" )) {
            throw new URISyntaxException( url, "HTTP/HTTPS is not supported, please use ws or wss instead" );
        }
        if( url.matches( "^//") ) {
            url = "ws:" + url;
        }
        else if( !url.matches( "^ws:|^wss:" )) {
            url = "ws://" + url;
        }
        URI uri = new URI(url);
        return uri;
    }

    @Override
    public void send(String message) {
        websocket.send( message );
    }

    @Override
    public void close() {
        websocket.close();
    }

    @Override
    public void open() {
/*        websocket = new WebSocket( originalURI, new Draft_10() );
        websocket.connect();*/
    }

    class WebSocket extends WebSocketClient {
        public WebSocket( URI serverUri , Draft draft  ) {
            super( serverUri, draft );
        }

        @Override
        public void onOpen(ServerHandshake handshakedata) {
            connection.onOpen();
        }

        @Override
        public void onMessage(String message) {
            connection.onMessage( message );
        }

        @Override
        public void onClose(int code, String reason, boolean remote) {
            try {
                connection.onClose();
            } catch( Exception e ) {
                System.err.println( e );
            }
        }

        @Override
        public void onError(Exception ex) {
            connection.onError( ex.getMessage() );
        }
    }
}