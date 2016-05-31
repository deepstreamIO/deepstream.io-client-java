package io.deepstream.util;


import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.util.ArrayList;

public class MockTcpServer {

    ServerSocket serverSocket;
    ArrayList<Thread> threads;
    ArrayList<String> messages;
    Socket lastSocket;
    InputStreamReader in;
    OutputStreamWriter out;

    public Boolean isOpen = false;

    public MockTcpServer( int port ) {
        threads = new ArrayList<>();
        messages = new ArrayList<>();

        try {
            serverSocket = new ServerSocket();
            serverSocket.setReuseAddress(true);
            serverSocket.setSoTimeout(20000);
            serverSocket.bind( new InetSocketAddress( port ) );
            isOpen = true;
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
        this.open();
    }

    public void open() {
        final MockTcpServer self = this;

        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    Socket sock = serverSocket.accept();
                    sock.setSoTimeout(20000);
                    self.lastSocket = sock;
                    self.handleConnection(sock);
                } catch (SocketException e) {
                    //Most likely thrown when closing the serverSocket
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }).start();

        try {
            Thread.sleep( 10 ); //Allow thread to open socket
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        return;
    }

    private void handleConnection( final Socket socket ) {
        final MockTcpServer self = this;

        try {
            in = new InputStreamReader(socket.getInputStream());
            out = new OutputStreamWriter(socket.getOutputStream());
        } catch (IOException e) {
            e.printStackTrace();
        }

        Thread connectionThread = new Thread() {
            @Override
            public void run() {
                while( socket.isClosed() == false ) {
                    try {
                        if( in.ready() ) {
                            char[] buffer = new char[ 1024 ];
                            int bytesRead = in.read( buffer, 0, 1024 );
                            self.messages.add( new String( buffer, 0, bytesRead ) );
                        }
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
                System.out.println( "Socket is now closed" );

                try {
                    this.join( 1 );
                } catch (InterruptedException e) {
                    e.printStackTrace();
                } finally {
                    self.threads.remove( this );
                }
            }
        };
        this.threads.add( connectionThread );
        connectionThread.start();
    }

    public void send( String message ) {
        try {
            this.out.write( message, 0, message.length() );
            this.out.flush();
        } catch (IOException e) {
            System.out.println( "Socket Has Been Closed By Client" );
        }
    }

    public void close() throws InterruptedException {
        try {
            for (Thread connectedThread : this.threads) {
                connectedThread.join(1);
            }
            this.serverSocket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        this.isOpen = false;
        this.threads = new ArrayList<>();
    }

    public String getLastMessage() {
        return this.messages.get( messages.size() - 1 );
    }

    public int getNumberOfConnections() {
        return this.threads.size();
    }

    public void resetMessageCount() { this.messages = new ArrayList<>(); }

    public int getMessageCount() { return this.messages.size(); }
}
