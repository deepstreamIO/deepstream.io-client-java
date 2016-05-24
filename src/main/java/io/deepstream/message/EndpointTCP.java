package io.deepstream.message;

import java.io.*;
import java.net.*;
import java.util.Map;

public class EndpointTCP implements Endpoint {

    private final String MPS = Character.toString( '\u001f' );
    private final String MS = Character.toString( '\u001e' );

    private Socket socket;
    private URL url;
    private Connection connection;
    private boolean isOpen;
    private String messageBuffer;

    private DataOutputStream out;
    private DataInputStream in;

    public EndpointTCP(String url, Map options, Connection connection) throws IOException {

        this.url = new URL( url );
        this.connection = connection;
        this.messageBuffer = "";
        this.socket = new Socket();
        this.socket.setSoTimeout(1);
        this.isOpen = false;
        this.open();

    }

    private void open() throws IOException {
        try {
            this.socket.connect(new InetSocketAddress( url.getHost(), url.getPort() ) );
            this.isOpen = true;
            this.connection.onOpen();
        } catch (Exception e) {
            this.onError(e);
            return;
        }

        try {
            this.in = new DataInputStream(this.socket.getInputStream());
            this.out = new DataOutputStream(this.socket.getOutputStream());
        } catch (Exception e) {
            this.onError(e);
            return;
        }
        this.run();
    }

    private void run() {
        final EndpointTCP self = this;

        new Thread(new Runnable() {
            @Override
            public void run() {
                while( self.isOpen ) {
                    try {
                        String message = self.in.readUTF();
                        self.onData( message );
                    } catch ( SocketTimeoutException se ) {
                    } catch ( EOFException se ) {
                    } catch (IOException e) {
                        self.onError( e );
                    }
                }
            }
        }).start();
    }

    private void onError( Exception e ) {
        String message;

        if( e instanceof ConnectException ) {
            message = String.format("Can\'t connect! Deepstream server unreachable on %s", this.url.getAuthority() );
        } else {
            message = e.getMessage();
        }
        connection.onError( message );
    }

    private void onData( String data ) {
        String message;

        // Incomplete message, write to buffer
        char lastChar = data.charAt( data.length() - 1 );
        if( !Character.toString( lastChar ).equals( MS ) ) {
            System.out.println("Incomplete message received...");
            this.messageBuffer += data;
            return;
        }

        // Message that completes previously received message
        if( this.messageBuffer.length() != 0 ) {
            message = this.messageBuffer + data;
            this.messageBuffer = "";

        } else {
            message = data;
        }

        this.connection.onMessage( message );
    }

    public void send(String message) {
        try {
            this.out.writeUTF( message );
        } catch (IOException e) {
            this.onError( e );
        }
    }
}
