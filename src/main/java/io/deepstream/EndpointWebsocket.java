package io.deepstream;

import com.neovisionaries.ws.client.*;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;
import java.util.List;

class EndpointWebsocket implements Endpoint {

    private final URI originalURI;
    private final Connection connection;
    private final WebSocketFactory webSocketFactory;
    private WebSocket websocket;

    EndpointWebsocket(String url, DeepstreamConfig deepstreamConfig, Connection connection ) throws URISyntaxException {
        this.originalURI = parseUri( url, deepstreamConfig.getPath() );
        this.connection = connection;
        this.webSocketFactory = new WebSocketFactory();
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
        else if( !(url.startsWith( "ws:" ) || url.startsWith( "wss:" )) ) {
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
        this.websocket.sendText( message );
    }

    @Override
    public void close() {
        this.websocket.disconnect();
    }

    @Override
    public void open() {
        try {
            this.websocket = this.webSocketFactory.createSocket( this.originalURI );
            this.websocket.addListener(new CustomSocket());
            this.websocket.connect();
        } catch (Exception e) {
            try {
                this.websocket.disconnect();
                connection.onClose();
            } catch (URISyntaxException e1) {
                e1.printStackTrace();
            }
        }
    }

    private class CustomSocket extends WebSocketAdapter {
        @Override
        public void onConnected(WebSocket websocket, Map<String, List<String>> headers) throws Exception {
            connection.onOpen();
        }

        @Override
        public void onDisconnected(WebSocket websocket, WebSocketFrame serverCloseFrame, WebSocketFrame clientCloseFrame, boolean closedByServer) throws Exception {
            connection.onClose();
        }

        @Override
        public void onTextMessage(WebSocket websocket, String text) throws Exception {
            connection.onMessage( text );
        }

        @Override
        public void onError(WebSocket websocket, WebSocketException cause) throws Exception {
            websocket.disconnect();
            connection.onError( cause.getMessage() );
        }
    }
}