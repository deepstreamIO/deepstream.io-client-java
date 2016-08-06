package io.deepstream;


import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.util.ArrayList;

public class MockTcpServer {

    public Boolean isOpen = false;
    ServerSocket serverSocket;
    ArrayList<Thread> threads;
    ArrayList<String> messages;
    Socket lastSocket;
    InputStreamReader in;
    OutputStreamWriter out;

    MockTcpServer( int port ) {
        System.out.println( "Creating new server localhost:" + port );
        threads = new ArrayList<>();
        messages = new ArrayList<>();

        try {
            serverSocket = new ServerSocket();
            serverSocket.setReuseAddress(true);
            serverSocket.bind( new InetSocketAddress( port ) );
            isOpen = true;
        } catch (IOException e) {
            e.printStackTrace();
        }
        this.open();
    }

    public void open() {
        final MockTcpServer self = this;

        new Thread(new Runnable() {
            @Override
            public void run() {
                while( serverSocket.isBound() ) {
                    try {
                        Socket sock = serverSocket.accept();
                        System.out.println( "New Connection" );
                        self.lastSocket = sock;
                        self.handleConnection(sock);
                    } catch (SocketException e) {
                        e.printStackTrace();
                        break;
                    } catch (IOException e) {
                        e.printStackTrace();
                        break;
                    }
                }
            }
        }).start();

        try {
            Thread.sleep( 10 ); //Allow thread to open socket
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
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
                while (!socket.isClosed()) {
                    try {
                        if( in.ready() ) {
                            char[] buffer = new char[ 1024 ];
                            int bytesRead = in.read( buffer, 0, 1024 );
                            if( bytesRead > -1 ) {
                                self.handleMessages( new String( buffer, 0, bytesRead ) );
                            } else {
                                System.out.println( "Lost connection 1" );
                                break;
                            }
                        }
                    } catch (IOException e) {
                        System.out.println( "Lost connection 2" );
                        break;
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

    private void handleMessages( String rawMsgs ) {
        String[] msgs = rawMsgs.split( "\u001e" );
        for (String m : msgs) {
            this.messages.add( m + "\u001e" );
        }
    }

    public void send( String message ) {
        try {
            this.out.write( message, 0, message.length() );
            this.out.flush();
        } catch (IOException e) {
            System.out.println( "Socket Has Been Closed By Client" );
        }
    }

    public void close()  {
        System.out.println( "Closing server.." );
        try {
            for (Thread connectedThread : this.threads) {
                connectedThread.join(1);
            }
            try {
                this.lastSocket.close();
            } catch (NullPointerException np) {
            }
            this.serverSocket.close();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        this.resetMessageCount();
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
