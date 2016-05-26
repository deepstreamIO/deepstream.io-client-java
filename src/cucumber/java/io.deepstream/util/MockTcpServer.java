package io.deepstream.util;


import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;

public class MockTcpServer {

    ServerSocket serverSocket;
    ArrayList<Thread> threads;
    ArrayList<String> messages;
    String lastMessage;
    Socket lastSocket;
    DataInputStream in;
    DataOutputStream out;

    public Boolean isOpen = false;

    public MockTcpServer( int port ) {
        threads = new ArrayList<>();
        messages = new ArrayList<>();

        try {
            serverSocket = new ServerSocket( port );
            isOpen = true;
        } catch (IOException e) {
            e.printStackTrace();
        }

        this.open();
    }

    public void open() {
        final MockTcpServer self = this;

        Thread thread = new Thread() {
            @Override
            public void run() {
            try {
                Socket sock = serverSocket.accept();
                self.lastSocket = sock;
                self.handleConnection(sock);
            } catch (IOException e) {
                //e.printStackTrace();
            }
            }
        };

        thread.start();

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
            in = new DataInputStream(socket.getInputStream());
            out = new DataOutputStream(socket.getOutputStream());
        } catch (IOException e) {
            e.printStackTrace();
        }

        Thread connectionThread = new Thread() {
            @Override
            public void run() {
                while( socket.isClosed() == false ) {
                    try {
                        if( in.available() > 0 ) {
                            String message = in.readUTF();
                            self.lastMessage = message;
                            self.messages.add( message );
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
        System.out.println( "Size:" + this.threads.size() );

        connectionThread.start();
    }

    public void send( String message ) {
        try {
            this.out.writeUTF( message );
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
        return lastMessage;
    }

    public int getNumberOfConnections() {
        return this.threads.size();
    }

    public void resetMessageCount() { this.messages = new ArrayList<>(); }

    public int getMessageCount() { return this.messages.size(); }
}
