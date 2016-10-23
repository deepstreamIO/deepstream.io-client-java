package io.deepstream;

import org.java_websocket.client.WebSocketClient;
import org.java_websocket.drafts.Draft;
import org.java_websocket.drafts.Draft_10;
import org.java_websocket.drafts.Draft_75;
import org.java_websocket.handshake.ServerHandshake;

import java.net.URI;
import java.net.URISyntaxException;

class EndpointWebsocket implements Endpoint {

    private final URI originalURI;
    private WebSocket websocket;
    private final Connection connection;

    EndpointWebsocket(String url, DeepstreamConfig deepstreamConfig, Connection connection ) throws URISyntaxException {
        this.originalURI = parseUri( url, deepstreamConfig.getPath() );
        this.connection = connection;
    }

     /**
      * Take the url passed when creating the client and ensure the correct
      * protocol is provided
      * @param  {String} url Url passed in by client
      * @param  {String} defaultPath Default path to concatenate if one doest not exist
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
        if( uri.getPath().equals("") ) {
            uri = uri.resolve( defaultPath );
        }
        return uri;
    }

    @Override
    public void send(String message) {
        this.websocket.send( message );
    }

    @Override
    public void close() {
        this.websocket.close();
    }

    @Override
    public void open() {
        this.websocket = new WebSocket( this.originalURI, new Draft_10() );
        this.websocket.connect();
    }

    private class WebSocket extends WebSocketClient {
        WebSocket( URI serverUri , Draft draft  ) {
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
            }
        }

        @Override
        public void onError(Exception ex) {
            connection.onError( ex.getMessage() );
        }
    }
}