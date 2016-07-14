package io.deepstream;


import java.io.IOException;
import java.net.URISyntaxException;

public class Context {

    String lastErrorMessage;
    static int GENERAL_TIMEOUT = 10;

    DeepstreamClient client;
    MockTcpServer server, server2;

    int serverPort = 9696;
    int server2port = 9898;

    public Context() throws InterruptedException, IOException, URISyntaxException {
        this.server = Singleton.getSingleton().getServer1();
        this.server2 = Singleton.getSingleton().getServer2();

        this.client = Singleton.getSingleton().getNewClient();
        Thread.sleep( 200 );
    }

    public String getUid() {
        return this.client.getUid();
    }

    public String getLastErrorMessage() {
        return Singleton.getSingleton().getLastErrorMessage();
    }
}
