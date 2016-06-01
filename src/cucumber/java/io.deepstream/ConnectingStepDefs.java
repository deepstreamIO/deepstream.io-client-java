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

public class ConnectingStepDefs {

    private DeepstreamClient client;
    Properties options = new Properties();
    LoginStatus status = new LoginStatus();
    DeepstreamException deepstreamException;

    @Given("^the client is initialised$")
    public void the_client_is_initialised() {
        options.setProperty( "endpoint", EndpointType.TCP.name() );
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
}
