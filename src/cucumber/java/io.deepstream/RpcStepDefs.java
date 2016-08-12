package io.deepstream;

import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import org.junit.Assert;

public class RpcStepDefs {

    DeepstreamClient client;
    int serverPort;
    int server2Port;
    int GENERAL_TIMEOUT = Context.GENERAL_TIMEOUT;

    RpcRequestedListenerMock toUpperCaseMock = new RpcRequestedListenerMock();

    RpcResult rpcResponse;
    RpcResponse rpcRequest;
    String response;
    String request;

    public RpcStepDefs(Context context) {
        this.client = context.client;
        this.serverPort = Context.serverPort;
        this.server2Port = Context.server2port;
    }

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
    public void client_makes_an_rpc(final String rpcName, final String data) throws InterruptedException {
        new Thread(new Runnable() {
            @Override
            public void run() {
                rpcResponse = client.rpc.make(rpcName, data);
            }
        }).start();

        Thread.sleep(20);
    }

    @When("^the client recieves an error RPC callback for \"([^\"]*)\" with the message \"([^\"]*)\"$")
    @Then("^the client recieves a successful RPC callback for \"(.*?)\" with data \"(.*?)\"$")
    public void client_receives_rpc( String rpcName, String data ) throws InterruptedException {
        Assert.assertEquals(data, rpcResponse.getData());
    }

    @Then("^the client recieves a request for a RPC called \"(.*?)\" with data \"(.*?)\"$")
    public void client_receives_request( String rpcName, String data ) throws InterruptedException {
        Assert.assertEquals( data, request );
    }

    @When("^the client responds to the RPC \"([^\"]*)\" with data \"([^\"]*)\"$")
    public void the_client_responds_to_the_RPC_with_data(String rpcName, String rpcData) throws Throwable {
        rpcRequest.send(rpcData);
        Thread.sleep(GENERAL_TIMEOUT * 2);
    }

    @When("^the client responds to the RPC \"([^\"]*)\" with the error \"([^\"]*)\"$")
    public void the_client_responds_to_the_RPC_with_the_error(String rpcName, String rpcError) throws Throwable {
        rpcRequest.error(rpcError);
        Thread.sleep(GENERAL_TIMEOUT * 2);
    }

    @When("^the client rejects the RPC \"([^\"]*)\"$")
    public void the_client_rejects_the_RPC(String arg1) throws Throwable {
        rpcRequest.reject();
        Thread.sleep(GENERAL_TIMEOUT * 2);
    }

    class RpcRequestedListenerMock implements RpcRequestedListener {
        @Override
        public void onRPCRequested(String rpcName, Object data, RpcResponse response) {
            request = (String) data;
            rpcRequest = response;
        }
    }
}
