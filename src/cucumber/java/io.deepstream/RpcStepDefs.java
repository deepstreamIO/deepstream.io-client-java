package io.deepstream;

import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import org.junit.Assert;

public class RpcStepDefs {

    DeepstreamClient client;
    int serverPort;
    int server2Port;
    int GENERAL_TIMEOUT = Context.GENERAL_TIMEOUT;

    public RpcStepDefs( Context context ) {
        this.client = context.client;
        this.serverPort = context.serverPort;
        this.server2Port = context.server2port;
    }

    RpcRequestedMock toUpperCaseMock = new RpcRequestedMock();
    ResponseCallback responseCallback = new ResponseCallback();

    RpcResponse rpcResponse;
    String response;
    String request;

    @Then("^the client provides a RPC called \"(.*?)\"$")
    public void the_client_provides_a_RPC_called( String rpcName ) throws InterruptedException {
        client.rpc.provide( rpcName, toUpperCaseMock );
        Thread.sleep(GENERAL_TIMEOUT);
    }

    @Then("^the client stops providing a RPC called \"(.*?)\"$")
    public void the_client_stops_providing_a_RPC_called( String rpcName ) throws InterruptedException {
        client.rpc.unprovide( rpcName );
        Thread.sleep(GENERAL_TIMEOUT);
    }

    @Then("^the client requests RPC \"(.*?)\" with data \"(.*?)\"$")
    public void client_makes_an_rpc( String rpcName, String data ) throws InterruptedException {
        client.rpc.make( rpcName, data, responseCallback );
        Thread.sleep(GENERAL_TIMEOUT);
    }

    @When("^the client recieves an error RPC callback for \"([^\"]*)\" with the message \"([^\"]*)\"$")
    @Then("^the client recieves a successful RPC callback for \"(.*?)\" with data \"(.*?)\"$")
    public void client_receives_rpc( String rpcName, String data ) throws InterruptedException {
        Assert.assertEquals( data, response );
    }

    @Then("^the client recieves a request for a RPC called \"(.*?)\" with data \"(.*?)\"$")
    public void client_receives_request( String rpcName, String data ) throws InterruptedException {
        Assert.assertEquals( data, request );
    }

    @When("^the client responds to the RPC \"([^\"]*)\" with data \"([^\"]*)\"$")
    public void the_client_responds_to_the_RPC_with_data(String rpcName, String rpcData) throws Throwable {
        rpcResponse.send( rpcData );
        Thread.sleep(GENERAL_TIMEOUT * 2);
    }

    @When("^the client responds to the RPC \"([^\"]*)\" with the error \"([^\"]*)\"$")
    public void the_client_responds_to_the_RPC_with_the_error(String rpcName, String rpcError) throws Throwable {
        rpcResponse.error( rpcError );
        Thread.sleep(GENERAL_TIMEOUT * 2);
    }

    @When("^the client rejects the RPC \"([^\"]*)\"$")
    public void the_client_rejects_the_RPC(String arg1) throws Throwable {
        rpcResponse.reject();
        Thread.sleep(GENERAL_TIMEOUT * 2);
    }

    class RpcRequestedMock implements RpcRequested {

        @Override
        public void Call(Object data, RpcResponse response) {
            request = (String) data;
            rpcResponse = response;
        }
    }

    class ResponseCallback implements RpcResponseCallback {

        @Override
        public void onData(Object data) {
            response = (String) data;
        }

        @Override
        public void onError(String err) {
            response = err;
        }
    }
}
