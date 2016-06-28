package io.deepstream;


import java.io.IOException;
import java.net.URISyntaxException;

public class Context {

    DeepstreamClient client;

    MockTcpServer server;
    MockTcpServer server2;

    int serverPort = 9696;
    int server2port = 9898;

    String uuid;

    public Context() throws IOException, URISyntaxException, InterruptedException {
        System.out.println( "Servers created" );
        server = new MockTcpServer( serverPort );
        server2 = new MockTcpServer( server2port );
        Thread.sleep(200);
        System.out.println( "client created");
        client = new DeepstreamClient( "localhost:" + serverPort );
        Thread.sleep(200);
    }

    public String getUuid() {
        return client.getUid();
    }
}
