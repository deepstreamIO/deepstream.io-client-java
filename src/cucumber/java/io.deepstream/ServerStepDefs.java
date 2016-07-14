package io.deepstream;

import cucumber.api.java.After;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import org.junit.Assert;

public class ServerStepDefs {

    private final char MPS =  '\u001f';
    private final char MS = '\u001e';

    private MockTcpServer server;
    int serverPort;
    private MockTcpServer server2;
    int server2Port;

    String clientUid;

    public ServerStepDefs( Context context ) {
        this.clientUid = context.getUuid();
        this.server = context.server;
        this.server2 = context.server2;
        this.serverPort = context.serverPort;
        this.server2Port = context.server2port;
    }

    @After
    public void afterScenario() {
        server.close();
        server2.close();
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
        //Thread.sleep(200);
        //Assert.assertEquals( connections, server.getNumberOfConnections() );
    }

    @Then("^the server sends the message (.*?)$")
    public void The_server_sends_the_message(String message) throws Throwable {
        if( message.contains( "<UID>" ) ) {
            message = message.replace( "<UID>", clientUid );
        }
        message = message.replace( '|', MPS );
        message = message.replace( '+', MS );
        server.send( message );
        Thread.sleep(200);
    }

    @Then("^the second server sends the message (.*?)$")
    public void The_second_server_sends_the_message(String message) throws Throwable {
        message = message.replace( '|', MPS );
        message = message.replace( '+', MS );
        server2.send( message );
        Thread.sleep(200);
    }

    @Then("^the last message the server recieved is (.*?)$")
    public void The_last_message_the_server_received_is( String message ) {
        String lastMsg = server.getLastMessage();
        System.out.println( lastMsg );
        System.out.println( Util.convertChars( message ));
        Assert.assertTrue( lastMsg.matches( Util.convertChars( message ) ) );
    }

    @Then("^the server received the message (.*?)$")
    public void server_received_message( String message ) throws InterruptedException {
        for ( String msg : server.messages) {
            if( msg.matches(Util.convertChars( message )) ) {
                Assert.assertTrue( true );
                return;
            }
        }
        Assert.assertTrue( false );
    }

    @Then("^the last message the second server recieved is (.*?)$")
    public void The_last_message_the_second_server_received_is( String message ) {
        Assert.assertEquals( message, Util.matchMessage( server2.getLastMessage() ) );
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
        //Assert.assertEquals( connections, server2.getNumberOfConnections() );
    }

    @When("^some time passes$")
    public void Time_passes() throws InterruptedException {
        Thread.sleep(3000);
    }

    @Given("^two seconds later$")
    public void two_seconds_later() throws InterruptedException {
        Thread.sleep(2000);
    }

    @When("^the connection to the server is lost$")
    public void connection_is_lost() throws InterruptedException {
        server.close();
        Thread.sleep(500);
    }

    @Given("^the client is on the second server$")
    public void the_client_is_on_the_second_server() throws InterruptedException {
    }

    @When("^the connection to the server is reestablished$")
    public void connection_is_reestablished$() throws InterruptedException {
        server = new MockTcpServer( serverPort );
        Thread.sleep(4000);
    }

    @When("^the server did not recieve any messages$")
    public void server_received_no_messages() throws InterruptedException {
        Assert.assertEquals( 0, server.getMessageCount() );
    }
}
