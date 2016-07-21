package io.deepstream;

import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import cucumber.api.java.en.Given;
import org.junit.Assert;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class RecordStepDefs {

    DeepstreamClient client;
    int serverPort;
    int server2Port;
    int GENERAL_TIMEOUT = Context.GENERAL_TIMEOUT;

    ListenCallback listenCallback = mock( ListenCallback.class );
    Record record;

    public RecordStepDefs( Context context ) {
        this.client = context.client;
        this.serverPort = context.serverPort;
        this.server2Port = context.server2port;
    }

    @Given("^the client deletes the record named \"([^\"]*)\"$")
    public void the_client_deletes_the_record_named(String recordName) throws Throwable {
        record.delete();
        Thread.sleep(GENERAL_TIMEOUT);
    }

    @When("^the client listens to a record matching \"([^\"]*)\"$")
    public void the_client_listens_to_a_record_matching(String pattern) throws Throwable {
        client.record.listen( pattern, listenCallback );
        Thread.sleep(GENERAL_TIMEOUT);
    }

    @When("^the client unlistens to a record matching \"([^\"]*)\"$")
    public void the_client_unlistens_to_a_record_matching(String pattern) throws Throwable {
        client.record.unlisten( pattern );
        Thread.sleep(GENERAL_TIMEOUT);
    }

    @When("^the client creates a record named \"([^\"]*)\"$")
    public void the_client_creates_a_record_named(String recordName ) throws Throwable {
        record = client.record.getRecord(recordName);
        Thread.sleep(GENERAL_TIMEOUT);
    }

    @When("^the client sets the record \"([^\"]*)\" to (.+)$")
    public void the_client_sets_the_record_to(String recordName, String value) throws Throwable {
        record.set( value );
        Thread.sleep(GENERAL_TIMEOUT);
    }

    @When("^the client sets the record \"([^\"]*)\" \"([^\"]*)\" to \"(.+)\"$")
    public void the_client_sets_the_record_to(String recordName, String path, String value) throws Throwable {
        record.set( path, value );
        Thread.sleep(GENERAL_TIMEOUT);
    }

    @When("^the client discards the record named \"([^\"]*)\"$")
    public void the_client_discards_the_record_named(String recordName) throws Throwable {
        record.discard();
        Thread.sleep(GENERAL_TIMEOUT);
    }

    @Then("^the client will be notified of new record match \"([^\"]*)\"$")
    public void the_client_will_be_notified_of_new_record_match(String recordName) throws Throwable {
        verify( listenCallback ).onSubscriptionForPatternAdded( recordName );
    }

    @Then("^the client will be notified of record match removal \"([^\"]*)\"$")
    public void the_client_will_be_notified_of_record_match_removal(String recordName) throws Throwable {
        verify( listenCallback ).onSubscriptionForPatternRemoved( recordName );
    }

    @Then("^the client record \"([^\"]*)\" data is (.*)$")
    public void the_client_record_data_is(String recordName, String data ) throws Throwable {
        Assert.assertEquals( data, record.get() );
    }

}
