package io.deepstream;


import org.java_websocket.WebSocket;
import org.java_websocket.handshake.ClientHandshake;
import org.java_websocket.server.WebSocketServer;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.*;
import java.util.ArrayList;

public class MockWebSocketServer {

    Boolean isOpen = false;
    WebsocketServer websocketServer;
    WebSocket lastSocket;

    MockWebSocketServer(int port ) {
        System.out.println("Creating new server localhost:" + port);

        try {
            this.websocketServer = new WebsocketServer(port);
        } catch (IOException e) {
            e.printStackTrace();
        }
        this.websocketServer.start();
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        this.isOpen = true;
    }

    public void send( String message ) {
        lastSocket.send( message );
    }

    public void close()  {
        System.out.println( "Closing server.." );
        this.resetMessageCount();
        try {
            this.websocketServer.stop();
        } catch (Exception e) {
            e.printStackTrace();
        }
        this.isOpen = false;
    }

    public String getLastMessage() {
        return this.websocketServer.getLastMessage();
    }

    public int getNumberOfConnections() {
        return this.websocketServer.connections;
    }

    public void resetMessageCount() {
        this.websocketServer.resetMessageCount();
    }

    public int getMessageCount() {
        return this.websocketServer.getMessageCount();
    }

    public ArrayList getMessages() {
        return this.websocketServer.messages;
    }

    class WebsocketServer extends WebSocketServer {

        Integer connections;
        ArrayList<String> messages;

        public WebsocketServer( int port ) throws UnknownHostException {
            super( new InetSocketAddress( port ) );
            this.connections = 0;
            this.resetMessageCount();
        }

        public String getLastMessage() {
            return this.messages.get( messages.size() - 1 );
        }

        public void resetMessageCount() { this.messages = new ArrayList<>(); }

        public int getMessageCount() { return this.messages.size(); }

        @Override
        public void onOpen(WebSocket conn, ClientHandshake handshake) {
            System.out.println( "Connection Opened" );
            lastSocket = conn;
            connections++;
        }

        @Override
        public void onClose(WebSocket conn, int code, String reason, boolean remote) {
            System.out.println( "Connection Closed" );
            connections--;
        }

        @Override
        public void onMessage(WebSocket conn, String message) {
            handleMessages( message );
        }

        @Override
        public void onError(WebSocket conn, Exception ex) {
            System.out.println( "Error" );
            System.out.println( ex );
        }

        private void handleMessages( String rawMsgs ) {
            String[] msgs = rawMsgs.split( "\u001e" );
            for (String m : msgs) {
                this.messages.add( m + "\u001e" );
            }
        }
    }
}
