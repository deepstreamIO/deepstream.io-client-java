package io.deepstream.connecting;

import cucumber.api.java.After;
import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import io.deepstream.DeepstreamClient;
import io.deepstream.MockTcpServer;
import io.deepstream.constants.EndpointType;
import io.deepstream.util.Util;
import org.json.JSONObject;
import org.junit.Assert;

import java.io.IOException;
import java.util.Properties;

public class ConnectionStepDefs {

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

    private DeepstreamClient client;
    Properties options = new Properties();

    @Before
    public void beforeScenario() throws IOException, InterruptedException {
        server.open();
        Thread.sleep(10);
    }

    @After
    public void afterScenario() throws IOException, InterruptedException {
        server.close();
    }

    @Given("^the test server is ready$")
    public void The_test_server_is_ready() throws Throwable {
        Assert.assertEquals( true, server.isOpen );
    }

    @Given("^the client is initialised$")
    public void the_client_is_initialised() throws Throwable {
        options.setProperty( "endpoint", EndpointType.TCP.name() );
        client = new DeepstreamClient("http://localhost:9696", options);
    }

    @Then("^the server has (\\d+) active connections$")
    public void The_server_has_connections(int connections) throws Throwable {
        Assert.assertEquals( connections, server.getNumberOfConnections() );
    }

    @Then("^the clients connection state is \"(.*?)\"$")
    public void the_clients_connection_state_is(String arg1) throws Throwable {
        Assert.assertEquals( arg1, client.getConnectionState().name() );
    }

    @Then("^the server sends the message (.*?)$")
    public void The_server_sends_the_message(String message) throws Throwable {
        message = message.replace( '|', MPS );
        message = message.replace( '+', MS );
        Thread.sleep(1000);
        server.send( message );
    }

    @When("^the client logs in with username \"(.*?)\" and password \"(.*?)\"")
    public void The_client_logs_in_with_username_and_password( String username, String password ) throws Exception {
        String creds = String.format("{\"username\":\"%s\", \"password\":\"%s\" }", username, password);
        JSONObject authData = new JSONObject( creds );
        client.login( authData );
    }

    @Then("^the last message the server recieved is (.*?)$")
    public void The_last_message_the_server_received_is( String message ) throws Exception {
        Assert.assertEquals( message, Util.matchMessage( server.getLastMessage() ) );
    }
}