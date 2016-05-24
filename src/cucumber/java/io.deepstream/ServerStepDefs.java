package io.deepstream;

import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import io.deepstream.util.MockTcpServer;
import io.deepstream.util.Util;
import org.junit.Assert;

import java.io.IOException;

public class ServerStepDefs {

    private final char MPS =  '\u001f';
    private final char MS = '\u001e';

    private MockTcpServer server;

    {
        try {
            server = new MockTcpServer(9696);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Before
    public void beforeScenario() throws IOException, InterruptedException {
        server.open();
    }

    @After
    public void afterScenario() throws IOException, InterruptedException {
        server.close();
    }

    @Given("^the test server is ready$")
    public void The_test_server_is_ready() throws Throwable {
        Assert.assertEquals( true, server.isOpen );
    }

    @Given("^the server resets its message count$")
    public void Server_resets_message_count() throws Throwable {
        server.resetMessageCount();
    }

    @Then("^the server has (\\d+) active connections$")
    public void The_server_has_connections(int connections) throws Throwable {
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
    public void The_last_message_the_server_received_is( String message ) throws Exception {
        Assert.assertEquals( message, Util.matchMessage( server.getLastMessage() ) );
    }

    @Then("^the server has received (\\d+) messages")
    public void Server_has_received_messages( int messageCount ) throws Exception {
        Assert.assertEquals( messageCount, server.getMessageCount() );
    }
}
