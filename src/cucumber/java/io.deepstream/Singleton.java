package io.deepstream;

import java.net.URISyntaxException;
import java.util.Properties;

class Singleton {

    private static Singleton singleton;
    private int serverPort = 7777;
    private int server2port = 8888;
    private String lastErrorMessage;
    private MockTcpServer server = new MockTcpServer( serverPort );
    private MockTcpServer server2 = new MockTcpServer( server2port );
    private DeepstreamClient client;

    private Singleton() {
    }

    static Singleton getSingleton() {
        if( singleton != null ) {
            return singleton;
        }
        System.out.println( "New Singleton" );
        singleton = new Singleton();
        return singleton;
    }

    MockTcpServer getServer1() {
        return this.server;
    }

    MockTcpServer getServer2() {
        return this.server2;
    }

    MockTcpServer getNewServer1() {
        this.server.close();
        this.server = new MockTcpServer( serverPort );
        return this.server;
    }

    DeepstreamClient getNewClient() throws URISyntaxException, InterruptedException, InvalidDeepstreamConfig {
        if( this.client != null ) {
            System.out.println( "Closing old client " );
            this.client.close();
            Thread.sleep( 100 );
        }

        System.out.println( "Creating new client" );
        Properties options = new Properties();
        options.put("endpointType", "tcp");
        options.put("subscriptionTimeout", "150");
        options.put("recordReadAckTimeout", "150");
        options.put("recordReadTimeout", "1000");
        options.put("recordDeleteTimeout", "150");
        options.put("rpcResponseTimeout", "500");
        options.put( "reconnectIntervalIncrement", "1500" );
        options.put("reconnectIntervalIncrement", "1500");
        options.put("maxReconnectAttempts", "1500");
        options.put("maxReconnectInterval", "1500");
        this.client = new DeepstreamClient( "localhost:" + serverPort, options );

        this.client.setRuntimeErrorHandler(new DeepstreamRuntimeErrorHandler() {
            @Override
            public void onException(Topic topic, Event event, String errorMessage) {
                System.out.println( "Uncaught error via the DeepstreamRuntimeErrorHandler: " + topic + " " + event + " " + errorMessage);
                lastErrorMessage = event + ": " + errorMessage;
            }
        });

        this.client.addConnectionChangeListener(new ConnectionStateListener() {
            @Override
            public void connectionStateChanged(ConnectionState connectionState) {
                System.out.println( "Connection state changed to: " + connectionState );
            }
        });

        lastErrorMessage = null;

        return this.client;
    }

    String getLastErrorMessage() {
        return lastErrorMessage;
    }
}
