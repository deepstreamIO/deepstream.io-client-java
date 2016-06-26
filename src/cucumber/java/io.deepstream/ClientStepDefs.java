package io.deepstream;

import com.google.gson.JsonObject;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import io.deepstream.constants.EndpointType;
import io.deepstream.constants.Event;
import io.deepstream.rpc.RpcRequested;
import io.deepstream.rpc.RpcResponse;
import io.deepstream.rpc.RpcResponseCallback;
import org.junit.Assert;

import java.util.Map;
import java.util.Properties;

public class ClientStepDefs {

    private DeepstreamClient client;
    Properties options = new Properties();
    LoginStatus status = new LoginStatus();
    DeepstreamException deepstreamException;

    @Given("^the client is initialised$")
    public void the_client_is_initialised() {
        options.setProperty( "endpoint", EndpointType.TCP.name() );
        options.setProperty( "debug", "true" );
        try {
            client = new DeepstreamClient( "localhost:9696", options );
            Thread.sleep(200);
        } catch( Exception e ) {
            Assert.fail( e.getMessage() );
        }
    }

    @Then("^the clients connection state is \"(.*?)\"$")
    public void the_clients_connection_state_is( String arg1 ) {
        Assert.assertEquals( arg1, client.getConnectionState().name() );
    }

    @When("^the client logs in with username \"(.*?)\" and password \"(.*?)\"")
    public void The_client_logs_in_with_username_and_password( String username, String password ) throws InterruptedException {
        JsonObject authData = new JsonObject();
        authData.addProperty( "password", password );
        authData.addProperty( "username", username );
        try {
            client.login(authData, status);
            Thread.sleep(500);
        } catch ( DeepstreamException ex ) {
            deepstreamException = ex;
        } catch (DeepstreamLoginException e) {
            e.printStackTrace();
        }
    }

    @Then("^the last login failed with error \"(.*?)\" and message \"(.*?)\"")
    public void The_last_login_failed_with_error_and_message( String expectedError, String expectedMessage ) {
        Assert.assertEquals( expectedError, status.errorEvent.name() );
        Assert.assertEquals( expectedMessage, status.errorMessage );
    }

    @Then("^the client throws a \"(.*?)\" error with message \"(.*?)\"")
    public void Client_throws_err_and_message( String expectedError, String expectedMessage ) {
        String message = deepstreamException.getMessage();
        Assert.assertTrue( message.contains( expectedError ));
        Assert.assertTrue( message.contains( expectedMessage ));
    }


    class LoginStatus implements LoginCallback {

        Event errorEvent;
        String errorMessage;

        @Override
        public void loginSuccess( Map loginData ) {
        }

        @Override
        public void loginFailed( Event errorEvent, Object errorMessage ) {
            this.errorEvent = errorEvent;
            this.errorMessage = errorMessage.toString();
        }
    }

    /**
     * Rpc step defs
     */

    RpcRequestedMock toUpperCaseMock = new RpcRequestedMock();
    ResponseCallback responseCallback = new ResponseCallback();
    String response;

    @Then("^the client provides a RPC called \"(.*?)\"$")
    public void the_client_provides_a_RPC_called( String rpcName ) throws InterruptedException {
        client.rpc.provide( rpcName, toUpperCaseMock );
        Thread.sleep(500);
    }

    @Then("^the client stops providing a RPC called \"(.*?)\"$")
    public void the_client_stops_providing_a_RPC_called( String rpcName ) throws InterruptedException {
        client.rpc.unprovide( rpcName );
        Thread.sleep(500);
    }

    @Then("^the client requests RPC \"(.*?)\" with data \"(.*?)\"$")
    public void client_makes_an_rpc( String rpcName, String data ) throws InterruptedException {
        client.rpc.make( rpcName, data, responseCallback );
        Thread.sleep(500);
    }

    @When("^the client recieves an error RPC callback for \"([^\"]*)\" with the message \"([^\"]*)\"$")
    @Then("^the client recieves a successful RPC callback for \"(.*?)\" with data \"(.*?)\"$")
    public void client_receives_rpc( String rpcName, String data ) throws InterruptedException {
        Assert.assertEquals( data, response );
    }

    class RpcRequestedMock implements RpcRequested {

        @Override
        public void Call(Object data, RpcResponse response) {
            String msg = (String) data;

            //Success
            if( msg.equals( "abc" ) ) {
                response.send( msg.toUpperCase() );
            }
            //Error
            else if( msg.equals( "def" ) ) {
                response.error( "An Error Occured" );
            }
            //Rejection when supported
            else if( msg.equals( "ghi" ) ) {
                response.reject();
            }
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
