package io.deepstream;


import com.google.gson.JsonObject;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import org.junit.Assert;

import java.util.Map;

public class ConnectingStepDefs {

    Context context;
    DeepstreamClient client;
    DeepstreamException deepstreamException;
    String errorMessage;
    LoginStatus status = new LoginStatus();
    int GENERAL_TIMEOUT = Context.GENERAL_TIMEOUT;

    public ConnectingStepDefs( Context context ) {
        this.context = context;
        this.client = context.client;
    }


    @Given("^the client is initialised$")
    public void the_client_is_initialised() {
        Assert.assertEquals(ConnectionState.AWAITING_CONNECTION, client.getConnectionState());
    }

    @Then("^the clients connection state is \"(.*?)\"$")
    public void the_clients_connection_state_is( String arg1 ) throws InterruptedException {
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


    @Then("^the last login was successful")
    public void The_last_login_was_successful() throws InterruptedException {
        Assert.assertTrue( status.success );
    }

    @Then("^the last login failed with error message \"(.*?)\"")
    public void The_last_login_failed_with_error_and_message( String expectedMessage ) throws InterruptedException {
        Thread.sleep(GENERAL_TIMEOUT);
        //Assert.assertEquals( expectedError, status.errorEvent.name() );
        Assert.assertEquals( expectedMessage, status.errorMessage );
    }

    @Then("^the client throws a \"(.*?)\" error with message \"(.*?)\"")
    public void Client_throws_err_and_message( String expectedError, String expectedMessage ) {
        String lastErrorMessage = context.getLastErrorMessage();
        Assert.assertNotNull( "Expected a runtime error to have been thrown", lastErrorMessage );
        Assert.assertTrue( "Expected error event: '" + lastErrorMessage + "' to contain '" + expectedError + "'", lastErrorMessage.contains( expectedError ));
        Assert.assertTrue( "Expected error message:'" + lastErrorMessage + "' to contain '" + expectedMessage + "'", lastErrorMessage.contains( expectedMessage ));
    }

    class LoginStatus implements LoginCallback {

        Event errorEvent;
        String errorMessage;
        Boolean success;

        @Override
        public void loginSuccess( Map loginData ) {
            this.success = true;
        }

        @Override
        public void loginFailed( Event errorEvent, Object errorMessage ) {
            this.errorEvent = errorEvent;
            this.errorMessage = errorMessage.toString();
        }
    }
}
