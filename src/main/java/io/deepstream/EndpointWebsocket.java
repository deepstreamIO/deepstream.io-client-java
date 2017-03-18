package io.deepstream;

import com.google.j2objc.annotations.J2ObjCIncompatible;
import org.java_websocket.client.DefaultSSLWebSocketClientFactory;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.drafts.Draft;
import org.java_websocket.drafts.Draft_10;
import org.java_websocket.handshake.ServerHandshake;

import javax.net.ssl.SSLContext;
import java.net.URI;
import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;


@J2ObjCIncompatible
class EndpointWebsocket implements Endpoint {

    private final URI uri;
    private WebSocket websocket;
    private final Connection connection;

    EndpointWebsocket(URI uri, Connection connection ) throws URISyntaxException {
        this.uri = uri;
        this.connection = connection;
    }

    @Override
    public void send(String message) {
        this.websocket.send( message );
    }

    @Override
    public void close() {
        this.websocket.close();
        this.websocket = null;
    }

    @Override
    public void forceClose() {
        this.websocket.getConnection().closeConnection(1, "Forcing connection close due to network loss");
    }

    @Override
    public void open() {
        this.websocket = new WebSocket( this.uri, new Draft_10() );
        this.websocket.connect();
    }

    private class WebSocket extends WebSocketClient {
        WebSocket( URI serverUri , Draft draft  ) {
            super( serverUri, draft );
            // Set the SSL context if the socket server is using Secure WebSockets
            if (serverUri.toString().startsWith("wss:")) {
                SSLContext sslContext;
                try {
                    sslContext = SSLContext.getInstance("TLS");
                    sslContext.init(null, null, null);
                } catch (NoSuchAlgorithmException | KeyManagementException e) {
                    throw new RuntimeException(e);
                }
                // set the SSL context to the client factory
                this.setWebSocketFactory(new DefaultSSLWebSocketClientFactory(sslContext));
            }
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
            if (ex instanceof NullPointerException && ex.getMessage().equals("ssl == null")) {
                return;
            }
            connection.onError( ex.getMessage() );
        }
    }
}
