package io.deepstream;

import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Properties;

public class Singleton {

    int serverPort = 9696;
    int server2port = 9898;

    String lastErrorMessage;
    MockTcpServer server = new MockTcpServer( serverPort );;
    MockTcpServer server2 = new MockTcpServer( server2port );;
    DeepstreamClient client;

    private static Singleton singleton;

    static Singleton getSingleton() {
        if( singleton != null ) {
            return singleton;
        }
        System.out.println( "New Singleton" );
        singleton = new Singleton();
        return singleton;
    }

    public Singleton() {
    }

    MockTcpServer getServer1() {
        return this.server;
    }

    MockTcpServer getServer2() {
        return this.server2;
    }

    DeepstreamClient getNewClient() throws IOException, URISyntaxException, InterruptedException {
        if( this.client != null ) {
            System.out.println( "Closing old client " );
            this.client.close();
            Thread.sleep( 100 );
        }

        System.out.println( "Creating new client" );
        Properties options = new Properties();
        options.put( "subscriptionTimeout", "100" );
        options.put( "recordReadAckTimeout", "200" );
        options.put( "recordReadTimeout", "260" );
        options.put( "recordDeleteTimeout", "100" );
        options.put( "rpcResponseTimeout", "200" );
        this.client = new DeepstreamClient( "localhost:" + serverPort, options );

        this.client.setRuntimeErrorHandler(new DeepstreamRuntimeErrorHandler() {
            @Override
            public void onException(Topic topic, Event event, String msg) {
                System.out.println( "Uncaught error via the DeepstreamRuntimeErrorHandler: " + topic + " " + event + " " +  msg );
                lastErrorMessage = event + ": " + msg;
            }
        });

        UtilAckTimeoutRegistry.resetAckTimeoutRegistry( this.client );

        lastErrorMessage = null;

        return this.client;
    }

    public String getLastErrorMessage() {
        return lastErrorMessage;
    }
}
