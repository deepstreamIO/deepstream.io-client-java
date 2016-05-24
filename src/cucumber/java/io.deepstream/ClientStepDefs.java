package io.deepstream;

import com.google.gson.JsonObject;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import io.deepstream.constants.EndpointType;
import io.deepstream.constants.Event;
import org.junit.Assert;

import java.util.Map;
import java.util.Properties;

public class ClientStepDefs {

    private DeepstreamClient client;
    Properties options = new Properties();
    LoginStatus status = new LoginStatus();
    DeepstreamException deepstreamException;

    @Given("^the client is initialised$")
    public void the_client_is_initialised() throws Throwable {
        options.setProperty( "endpoint", EndpointType.TCP.name() );
        client = new DeepstreamClient( "http://localhost:9696", options );
    }

    @Then("^the clients connection state is \"(.*?)\"$")
    public void the_clients_connection_state_is( String arg1 ) throws Throwable {
        Assert.assertEquals( arg1, client.getConnectionState().name() );
    }

    @When("^the client logs in with username \"(.*?)\" and password \"(.*?)\"")
    public void The_client_logs_in_with_username_and_password( String username, String password ) throws Exception {
        JsonObject authData = new JsonObject();
        authData.addProperty( "password", password );
        authData.addProperty( "username", username );
        try {
            client.login(authData, status);
        } catch ( DeepstreamException ex ) {
            deepstreamException = ex;
        }
    }

    @Then("^the last login failed with error \"(.*?)\" and message \"(.*?)\"")
    public void The_last_login_failed_with_error_and_message( String expectedError, String expectedMessage ) throws Exception {
        Assert.assertEquals( expectedError, status.errorEvent.name() );
        Assert.assertEquals( expectedMessage, status.errorMessage );
    }

    @Then("^the client throws a \"(.*?)\" error with message \"(.*?)\"")
    public void Client_throws_err_and_message( String expectedError, String expectedMessage ) throws Exception {
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
        public void loginFailed( Event errorEvent, String errorMessage ) {
            this.errorEvent = errorEvent;
            this.errorMessage = errorMessage;
        }
    }
}
