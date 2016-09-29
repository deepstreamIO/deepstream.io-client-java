package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.ConnectException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.URISyntaxException;

/**
 * An implementation of {@link Endpoint} that allows use to interact with with deepstream via TCP. This provides much
 * better speed, but does mean you'll have to be careful which firewalls lie inbetween.
 */
class EndpointTCP implements Endpoint {

    private final String MPS = Character.toString( '\u001f' );
    private final String MS = Character.toString( '\u001e' );
    private final String host;
    private final Integer port;
    private final Connection connection;
    private Socket socket;
    private String messageBuffer;
    private boolean closed;

    private OutputStreamWriter out;
    private InputStreamReader in;

    @ObjectiveCName("init:deepstreamConfig:connection:")
    public EndpointTCP(String url, DeepstreamConfig deepstreamConfig, Connection connection) throws URISyntaxException {
        try {
            this.host = url.substring( 0, url.indexOf( ':' ) );
            this.port = Integer.parseInt( url.substring( url.indexOf( ':' ) + 1 )  );
        } catch( Exception e ) {
            throw new URISyntaxException( url, "URL provided is not correct" );
        }

        this.connection = connection;

        this.messageBuffer = "";

        this.open();
    }

    public void open() {
        try {
            this.socket = new Socket();
            this.socket.setKeepAlive( true );
            this.socket.connect(new InetSocketAddress( host, port ));
        } catch (IOException e) {
            this.onError( e );
            return;
        }

        try {
            this.in = new InputStreamReader( this.socket.getInputStream() );
            this.out = new OutputStreamWriter( this.socket.getOutputStream() );
            this.connection.onOpen();
        } catch (IOException e) {
            onError( new ConnectException() );
            return;
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
                        char[] buffer = new char[ 1024 ];
                        int bytesRead = in.read( buffer, 0, 1024 );

                        if( bytesRead == -1 ) {
                            self.onError( new ConnectException() );
                            return;
                        }
                        self.onData( new String( buffer, 0, bytesRead ) );
                    } catch ( IOException e ) {
                        if( !self.closed ) {
                            self.onError( e );
                        }
                    }
                }
            }
        }).start();
    }

     @ObjectiveCName("onError:")
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

    @ObjectiveCName("send:")
    public void send(String message) {
        try {
            this.out.write( message, 0, message.length() );
            this.out.flush();
        } catch (IOException e) {
            if( !this.closed ) {
                this.onError( e );
            }
        }
    }

    public void close() {
        this.closed = true;

        try {
            this.socket.close();
        } catch ( IOException e ) {
            e.printStackTrace();
        }

        try {
            this.connection.onClose();
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
    }
}
