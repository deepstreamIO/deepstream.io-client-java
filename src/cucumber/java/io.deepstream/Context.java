package io.deepstream;


import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Properties;

public class Context {

    String lastErrorMessage;
    DeepstreamClient client;

    int serverPort = 9696;
    int server2port = 9898;

    static int GENERAL_TIMEOUT = 50;

    MockTcpServer server = new MockTcpServer( serverPort );
    MockTcpServer server2 = new MockTcpServer( server2port );

    public Context() throws InterruptedException, IOException, URISyntaxException {
        this.start();
    }

    void start() throws IOException, URISyntaxException, InterruptedException {
        if( client != null && client.getConnectionState() != ConnectionState.CLOSED ) {
            client.close();
        }

        System.out.println( "client created");

        Properties options = new Properties();
        options.put( "subscriptionTimeout", "100" );
        options.put( "recordReadAckTimeout", "200" );
        options.put( "recordReadTimeout", "260" );
        options.put( "recordDeleteTimeout", "100" );
        options.put( "rpcResponseTimeout", "200" );
        client = new DeepstreamClient( "localhost:" + serverPort, options );

        client.setRuntimeErrorHandler(new DeepstreamRuntimeErrorHandler() {
            @Override
            public void onException(Topic topic, Event event, String msg) {
                System.out.println( "Uncaught error via the DeepstreamRuntimeErrorHandler: " + topic + " " + event + " " +  msg );
                lastErrorMessage =  event + ": " + msg;
            }
        });

        Thread.sleep(100);

    }

    void stop() throws InterruptedException {
        client.close();
        Thread.sleep(200);
    }

    public String getUid() {
        return client.getUid();
    }
}
