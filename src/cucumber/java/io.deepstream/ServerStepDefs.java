package io.deepstream;

import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import io.deepstream.util.MockTcpServer;
import io.deepstream.util.Util;
import org.junit.Assert;

import java.io.IOException;

public class ServerStepDefs {

    private final char MPS =  '\u001f';
    private final char MS = '\u001e';

    private MockTcpServer server;
    private MockTcpServer server2;

    @Before
    public void beforeScenario() throws InterruptedException, IOException {
        server = new MockTcpServer(9696);
        server2 = new MockTcpServer(8898);
    }

    @After
    public void afterScenario() throws InterruptedException, IOException {
        server.close();
        server2.close();
    }

    @Given("^the test server is ready$")
    public void The_test_server_is_ready() throws Throwable {
        Assert.assertTrue( server.isOpen );
    }

    @Given("^the server resets its message count$")
    public void Server_resets_message_count() throws Throwable {
        server.resetMessageCount();
    }

    @Then("^the server has (\\d+) active connections$")
    public void The_server_has_connections(int connections) throws Throwable {
        Thread.sleep(200);
        Assert.assertEquals( connections, server.getNumberOfConnections() );
    }

    @Then("^the server sends the message (.*?)$")
    public void The_server_sends_the_message(String message) throws Throwable {
        Thread.sleep(200);
        message = message.replace( '|', MPS );
        message = message.replace( '+', MS );
        server.send( message );
        Thread.sleep(200);
    }

    @Then("^the last message the server recieved is (.*?)$")
    public void The_last_message_the_server_received_is( String message ) {
        Assert.assertEquals( message, Util.matchMessage( server.getLastMessage() ) );
    }

    @Then("^the server has received (\\d+) messages")
    public void Server_has_received_messages( int messageCount ) {
        Assert.assertEquals( messageCount, server.getMessageCount() );
    }

    @Given("^the second test server is ready$")
    public void Second_server_ready() throws Throwable {
        Assert.assertTrue( server2.isOpen );
    }

    @Then("^the second server has (\\d+) active connections$")
    public void Second_server_has_connections(int connections) {
        Assert.assertEquals( connections, server2.getNumberOfConnections() );
    }

    @When("^some time passes$")
    public void Time_passes() throws InterruptedException {
        Thread.sleep(500);
    }
}
