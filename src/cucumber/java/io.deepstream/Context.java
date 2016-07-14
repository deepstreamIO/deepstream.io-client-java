package io.deepstream;


import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Properties;

public class Context {

    String lastErrorMessage;
    DeepstreamClient client;

    MockTcpServer server;
    MockTcpServer server2;

    int serverPort = 9696;
    int server2port = 9898;

    static int GENERAL_TIMEOUT = 50;

    String uuid;

    public Context() throws IOException, URISyntaxException, InterruptedException {
        System.out.println( "Servers created" );
        server = new MockTcpServer( serverPort );
        server2 = new MockTcpServer( server2port );
        Thread.sleep(200);
        System.out.println( "client created");

        Properties options = new Properties();
        options.put( "subscriptionTimeout", "100" );
        options.put( "recordReadAckTimeout", "200" );
        options.put( "recordReadTimeout", "260" );
        options.put( "recordDeleteTimeout", "100" );
        options.put( "rpcResponseTimeout", "200" );
        client = new DeepstreamClient( "localhost:" + serverPort, options );

        Thread.setDefaultUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler() {
            public void uncaughtException(Thread t, Throwable e) {
                lastErrorMessage = e.getMessage();
            }
        });

        Thread.sleep(200);
    }

    public String getUid() {
        return client.getUid();
    }
}
