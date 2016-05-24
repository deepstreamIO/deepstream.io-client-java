package io.deepstream.util;


import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.*;
import java.util.ArrayList;

public class MockTcpServer {

    ServerSocket serverSocket;
    ArrayList<Socket> connections;
    ArrayList<Thread> threads;
    String lastMessage;
    Socket lastSocket;
    DataInputStream in;
    DataOutputStream out;
    public boolean isOpen;

    public MockTcpServer( int port ) throws IOException {
        serverSocket = new ServerSocket();
        serverSocket.setReuseAddress( true );
        serverSocket.bind(new InetSocketAddress(port));
        connections = new ArrayList<>();
        threads = new ArrayList<>();
    }

    public void open() throws InterruptedException {
        this.isOpen = true;
        final MockTcpServer self = this;
        Thread thread = new Thread() {
            @Override
            public void run() {
                try {
                    Socket sock = serverSocket.accept();
                    self.lastSocket = sock;
                    self.handleConnection(sock);
                } catch ( SocketTimeoutException e) {
                    System.out.println( "SocketTimeoutException " + e );
                } catch ( SocketException e ) {
                    System.out.println( "SocketException " + e );
                } catch (IOException e) {
                    System.out.println( "IOException " + e );
                }
            }
        };
        thread.start();
        Thread.sleep(10); //Allow thread to open socket
        return;
    }

    private void handleConnection( final Socket sock ) {
        this.connections.add( sock );
        final MockTcpServer self = this;

        Thread connectionThread = new Thread() {
            @Override
            public void run() {
                while( self.isOpen ) {
                    try {
                        self.in = new DataInputStream(sock.getInputStream());
                        self.out = new DataOutputStream(sock.getOutputStream());
                    } catch (IOException e) {
                        System.out.println( "IOException " + e );
                    }

                    try {
                        String message = in.readUTF();
                        self.lastMessage = message;
                    } catch ( SocketException e) {
                        System.out.println( "SocketException " + e );
                    } catch (IOException e) {
                        System.out.println( "IOException " + e );
                    }
                }
            }
        };
        this.threads.add( connectionThread );
        connectionThread.start();
    }

    public void send( String message ) throws IOException {
        this.out.writeUTF( message );
    }

    public void close() throws InterruptedException, IOException {
        this.isOpen = false;
        for ( Socket sock : this.connections ) {
            sock.close();
        }
        for ( Thread connectedThread : this.threads ) {
            connectedThread.join(1);
        }
        serverSocket.close();
        serverSocket = null;
    }

    public String getLastMessage() {
        return lastMessage;
    }

    public int getNumberOfConnections() {
        return this.connections.size();
    }
}
