package io.deepstream.util;


import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.*;
import java.util.ArrayList;

public class MockTcpServer {

    private ServerSocket serverSocket;
    private ArrayList<Socket> connections;
    private ArrayList<Thread> threads;
    private String lastMessage;
    private Socket lastSocket;
    private DataInputStream in;
    private DataOutputStream out;
    public boolean isOpen;

    public MockTcpServer( int port ) throws IOException {
        serverSocket = new ServerSocket();
        serverSocket.setReuseAddress( true );
        serverSocket.bind(new InetSocketAddress(port));
        connections = new ArrayList<>();
        threads = new ArrayList<>();
    }

    public void open() {
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
    }

    private void handleConnection( final Socket socket ) {
        this.connections.add( socket );
        final MockTcpServer self = this;

        Thread connectionThread = new Thread() {
            @Override
            public void run() {
                try {
                    self.in = new DataInputStream(socket.getInputStream());
                    self.out = new DataOutputStream(socket.getOutputStream());
                }
                catch (IOException e) {
                    System.out.println( "IOException " + e );
                }

                while( self.isOpen ) {
                    try {
                        self.lastMessage = in.readUTF();
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
