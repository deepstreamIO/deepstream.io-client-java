package io.deepstream;

import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import io.deepstream.utils.Util;
import org.junit.Assert;

import java.io.IOException;

public class ServerStepDefs {

    private final char MPS =  '\u001f';
    private final char MS = '\u001e';

    private MockTcpServer server;
    private MockTcpServer server2;

    String clientUid = "";

    @Before
    public void beforeScenario() throws InterruptedException, IOException {
        server = new MockTcpServer(9696);
        server2 = new MockTcpServer(8898);
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
    public void The_server_has_connections(int connections) {
        Assert.assertEquals( connections, server.getNumberOfConnections() );
    }

    @Then("^the server sends the message (.*?)$")
    public void The_server_sends_the_message(String message) throws Throwable {
        if( message.contains( "<UID>" ) ) {
            message = message.replace( "<UID>", ClientStepDefs.client.getUid() );
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
        Assert.assertTrue( lastMsg.matches( Util.convertChars( message ) ) );
    }

    @Then("^the server received the message (.*?)$")
    public void server_received_message( String message ) {
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
        Assert.assertEquals( connections, server2.getNumberOfConnections() );
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

    @When("^the connection to the server is reestablished$")
    public void connection_is_reestablished$() throws InterruptedException {
        server = new MockTcpServer( 9696 );
        Thread.sleep(5000);
    }
}
