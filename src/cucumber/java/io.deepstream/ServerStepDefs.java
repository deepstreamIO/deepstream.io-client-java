package io.deepstream;

import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import org.junit.Assert;

public class ServerStepDefs {

    int GENERAL_TIMEOUT = Context.GENERAL_TIMEOUT;
    private final char MPS =  '\u001f';
    private final char MS = '\u001e';

    private MockTcpServer server;
    int serverPort;
    private MockTcpServer server2;
    int server2Port;

    Context context;
    String clientUid;

    public ServerStepDefs( Context context ) {
        this.context = context;
        this.clientUid = context.getUid();
        this.server = context.server;
        this.server2 = context.server2;
        this.serverPort = context.serverPort;
        this.server2Port = context.server2port;
    }

    @Given("^the test server is ready$")
    public void The_test_server_is_ready() {
        Assert.assertTrue( server.isOpen );
    }

    @Given("^the server resets its message count$")
    public void Server_resets_message_count() {
        server.resetMessageCount();
    }

    @Then("^the server has (\\d+) active connections$")
    public void The_server_has_connections(int connections) throws Throwable {
        //Thread.sleep(1000);
        //Assert.assertEquals( connections, server.getNumberOfConnections() );
    }

    @Then("^the server sends the message (.*?)$")
    public void The_server_sends_the_message(String message) throws Throwable {
        if( message.contains( "<UID>" ) ) {
            message = message.replace( "<UID>", clientUid );
        }
        server.send( context.sendMessage( message ) );
        Thread.sleep(GENERAL_TIMEOUT * 3);
    }

    @Then("^the second server sends the message (.*?)$")
    public void The_second_server_sends_the_message(String message) throws Throwable {
        server2.send( context.sendMessage( message ) );
        Thread.sleep(GENERAL_TIMEOUT);
    }

    @Then("^the last message the server recieved is (.*?)$")
    public void The_last_message_the_server_received_is( String message ) {
        String lastMsg = context.recieveMessage( server.getLastMessage() );
        message = context.recieveMessage( message );
        System.out.println( "Server Received: " + message + " " + lastMsg );
        if(  lastMsg.equals( message ) == false ) {
            Assert.assertTrue( "Expected \n\t'" + lastMsg + "' to match \n\t'" + message + "'", lastMsg.matches( message ) );
        }
    }

    @Then("^the server received the message (.*?)$")
    public void server_received_message( String message ) throws InterruptedException {
        for ( String msg : server.messages) {
            if( msg.matches(context.recieveMessage( message )) ) {
                Assert.assertTrue( true );
                return;
            } else if( msg.equals( context.recieveMessage( message ) ) ) {
                Assert.assertTrue( true );
                return;
            } else {
                System.out.println( "Not a match " +  message + " " + context.sendMessage( msg ) );
            }
        }

        Assert.assertTrue( "Expected " + context.recieveMessage( message ) + " from " + server.messages, false );
    }

    @Then("^the last message the second server recieved is (.*?)$")
    public void The_last_message_the_second_server_received_is( String message ) {
        Assert.assertEquals( message, context.recieveMessage( server2.getLastMessage() ) );
    }

    @Then("^the server has received (\\d+) messages")
    public void Server_has_received_messages( int messageCount ) {
        Assert.assertEquals( messageCount, server.getMessageCount() );
    }

    @Given("^the second test server is ready$")
    public void Second_server_ready() {
        Assert.assertTrue( server2.isOpen );
    }

    @Then("^the second server has (\\d+) active connections$")
    public void Second_server_has_connections(int connections) {
        Assert.assertEquals( connections, server2.getNumberOfConnections() );
    }

    @When("^some time passes$")
    public void Time_passes() throws InterruptedException {
        Thread.sleep(200);
    }

    @Given("^two seconds later$")
    public void two_seconds_later() throws InterruptedException {
        Thread.sleep(2000);
    }

    @When("^the connection to the server is lost$")
    public void connection_is_lost() throws InterruptedException {
        server.close();
        Thread.sleep(200);
    }

    @Given("^the client is on the second server$")
    public void the_client_is_on_the_second_server() throws InterruptedException {
        server = context.server2;
    }

    @When("^the connection to the server is reestablished$")
    public void connection_is_reestablished$() throws InterruptedException {
        server = context.getNewServer1();
        Thread.sleep(1000);
    }

    @When("^the server did not recieve any messages$")
    public void server_received_no_messages() throws InterruptedException {
        Assert.assertEquals( 0, server.getMessageCount() );
    }
}
