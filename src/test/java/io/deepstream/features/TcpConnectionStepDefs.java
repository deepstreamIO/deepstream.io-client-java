package io.deepstream.features;

import cucumber.api.java.After;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import io.deepstream.DeepstreamClient;
import io.deepstream.MockTcpServer;
import io.deepstream.constants.EndpointType;
import org.junit.Assert;

import java.io.IOException;
import java.util.Properties;

public class TcpConnectionStepDefs {

    private MockTcpServer server;
    private DeepstreamClient client;
    Properties options = new Properties();

    @After
    public void afterScenario() throws IOException, InterruptedException {
        server.close();
    }

    @Given("^the test server is ready$")
    public void The_test_server_is_ready() throws Throwable {
        server = new MockTcpServer( 9696 );
        Assert.assertEquals( true, server.isOpen );
    }

    @Given("^the client is initialised$")
    public void the_client_is_initialised() throws Throwable {
        options.setProperty( "endpoint", EndpointType.TCP.name() );
        client = new DeepstreamClient("http://localhost:9696", options);
    }

    @Then("^the server has (\\d+) active connections")
    public void The_server_has_connections(int connections) throws Throwable {
        Assert.assertEquals( connections, server.getNumberOfConnections() );
    }

    @Then("^the clients connection state is \"(.*?)\"$")
    public void the_clients_connection_state_is(String arg1) throws Throwable {
        Assert.assertEquals( arg1, client.getConnectionState().name() );
    }
}