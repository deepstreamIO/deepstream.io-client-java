package io.deepstream;


import java.io.IOException;
import java.net.URISyntaxException;

public class Context {

    private static final char MPS =  '\u001f';
    private static final char MS = '\u001e';
    static int GENERAL_TIMEOUT = 50;
    static int serverPort = 7777;
    static int server2port = 8888;
    DeepstreamClient client;
    MockTcpServer server, server2;

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

    public String sendMessage(String input ) {
        return input
                .replaceAll( "<FIRST_SERVER_URL>", "localhost:" + Context.serverPort )
                .replaceAll( "<SECOND_SERVER_URL>", "localhost:" + Context.server2port )
                .replace( '|', MPS )
                .replace( '+', MS );
    }

    public String recieveMessage(String input ) {
        return input
                .replace( "{", "\\{" )
                .replace( "}", "\\}" )
                .replaceAll( "<UID>", "[a-z0-9]{8}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{12}")
                .replaceAll( "<FIRST_SERVER_URL>", "localhost:[0-9]{4}")
                .replace( '|', MPS )
                .replace( '+', MS );
    }

    public MockTcpServer getNewServer1() {
        this.server = Singleton.getSingleton().getNewServer1();
        return this.server;
    }
}
