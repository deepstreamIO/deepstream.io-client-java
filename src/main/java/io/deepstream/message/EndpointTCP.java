package io.deepstream.message;

import java.io.*;
import java.net.*;
import java.util.Map;

public class EndpointTCP implements Endpoint {

    private final String MPS = Character.toString( '\u001f' );
    private final String MS = Character.toString( '\u001e' );

    private Socket socket;
    private String host;
    private Integer port;
    private Connection connection;
    private String messageBuffer;

    private DataOutputStream out;
    private DataInputStream in;

    public EndpointTCP(String url, Map options, Connection connection) throws URISyntaxException {
        this.host = url.substring( 0, url.indexOf( ':' ) );
        this.port = Integer.parseInt( url.substring( url.indexOf( ':' ) + 1 )  );

        this.connection = connection;
        this.messageBuffer = "";

        this.open();
    }

    public void open() {
        try {
            this.socket = new Socket();
            this.socket.setSoTimeout(10);
            this.socket.connect(new InetSocketAddress( host, port ));
            this.connection.onOpen();
        } catch (IOException e) {
            this.onError( e );
            return;
        }

        try {
            this.in = new DataInputStream(this.socket.getInputStream());
            this.out = new DataOutputStream(this.socket.getOutputStream());
        } catch (IOException e) {
            e.printStackTrace();
        }

        this.run();
    }

    private void run() {
        final EndpointTCP self = this;

        new Thread(new Runnable() {
            @Override
            public void run() {
                while( !self.socket.isClosed() ) {
                    try {
                        String message = self.in.readUTF();
                        self.onData( message );
                    } catch ( SocketTimeoutException se ) {
                    } catch ( IOException e ) {
                        self.onError( e );
                    }
                }
            }
        }).start();
    }

    private void onError( Exception e ) {
        String message;

        if( e instanceof ConnectException || e instanceof EOFException ) {
            message = String.format( "Can\'t connect! Deepstream server unreachable on %s:%s", this.host, this.port );
        } else {
            message = e.getMessage();
        }
        connection.onError( message );
        this.close();
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

    public void close() {
        try {
            this.socket.shutdownInput();
            this.socket.shutdownOutput();
            this.socket.close();
        } catch ( IOException e ) {}

        try {
            this.connection.onClose();
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
    }
}
