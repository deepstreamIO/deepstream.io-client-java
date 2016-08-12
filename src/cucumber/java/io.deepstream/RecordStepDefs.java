package io.deepstream;

import com.google.gson.Gson;
import com.google.gson.JsonElement;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import org.junit.Assert;
import org.mockito.Matchers;

import static org.mockito.Mockito.*;
import static org.mockito.internal.verification.VerificationModeFactory.times;

public class RecordStepDefs {

    Gson gson = new Gson();
    DeepstreamClient client;
    int serverPort;
    int server2Port;
    int GENERAL_TIMEOUT = Context.GENERAL_TIMEOUT;

    ListenListener listenCallback = mock( ListenListener.class );
    RecordChangedCallback recordChangedCallback = mock( RecordChangedCallback.class );
    RecordPathChangedCallback recordPathChangedCallback = mock( RecordPathChangedCallback.class );
    Record record;

    JsonElement snapshotData;
    boolean hasRecord;
    DeepstreamError hasRecordError;
    DeepstreamError snapshotError;

    public RecordStepDefs( Context context ) {
        this.client = context.client;
        this.serverPort = Context.serverPort;
        this.server2Port = Context.server2port;
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
    public void the_client_creates_a_record_named(final String recordName) throws Throwable {
        new Thread(new Runnable() {
            @Override
            public void run() {
                record = client.record.getRecord(recordName);
            }
        }).start();

        Thread.sleep(GENERAL_TIMEOUT);
    }

    @When("^the client sets the record \"([^\"]*)\" to (.+)$")
    public void the_client_sets_the_record_to(String recordName, String value) throws Throwable {
        record.set( gson.fromJson( value, JsonElement.class) );
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
        Assert.assertEquals( gson.fromJson( data, JsonElement.class ), record.get() );
    }

    /**
     * Subscriptions
     */

    @When("^the client subscribes to the entire record \"([^\"]*)\" changes$")
    public void the_client_subscribes_to_the_entire_record_changes(String recordName) throws Throwable {
        record.subscribe( recordChangedCallback );
        Thread.sleep(GENERAL_TIMEOUT);
    }

    @Then("^the client will not be notified of the record change$")
    public void the_client_will_not_be_notified_of_the_record_change() throws Throwable {
        verify( recordChangedCallback, times( 0) ).onRecordChanged(Matchers.anyString(), Matchers.any(JsonElement.class));
        verify( recordPathChangedCallback, times( 0) ).onRecordPathChanged(Matchers.anyString(), Matchers.anyString(), Matchers.any(JsonElement.class));
        reset( recordChangedCallback );
    }

    @Then("^the client will be notified of the record change$")
    public void the_client_will_be_notified_of_the_record_change() throws Throwable {
        try {
            verify( recordChangedCallback, times( 1) ).onRecordChanged(Matchers.anyString(), Matchers.any(JsonElement.class));
        } catch( Throwable e ) {
            verify( recordPathChangedCallback, times( 1) ).onRecordPathChanged(Matchers.anyString(), Matchers.anyString(), Matchers.any(JsonElement.class));
        }

        reset( recordChangedCallback );
        reset( recordPathChangedCallback );
    }

    @Then("^the client will be notified of the partial record change$")
    public void the_client_will_be_notified_of_the_partial_record_change() throws Throwable {
        the_client_will_be_notified_of_the_record_change();
    }

    @Given("^the client unsubscribes to the entire record \"([^\"]*)\" changes$")
    public void the_client_unsubscribes_to_the_entire_record_changes(String recordName) throws Throwable {
        record.unsubscribe( recordChangedCallback );
        Thread.sleep(GENERAL_TIMEOUT);
        reset( recordChangedCallback );
        reset( recordPathChangedCallback );
    }

    @When("^the client subscribes to \"([^\"]*)\" for the record \"([^\"]*)\"$")
    public void the_client_subscribes_to_for_the_record(String path, String recordName) throws Throwable {
        record.subscribe( path, recordPathChangedCallback );
        Thread.sleep(GENERAL_TIMEOUT);
        reset( recordChangedCallback );
        reset( recordPathChangedCallback );
    }

    @Then("^the client will be notified of the second record change$")
    public void the_client_will_be_notified_of_the_second_record_change() throws Throwable {
        //TODO
    }

    @Given("^the client unsubscribes to \"([^\"]*)\" for the record \"([^\"]*)\"$")
    public void the_client_unsubscribes_to_for_the_record(String path, String recordName) throws Throwable {
        record.unsubscribe( path, recordPathChangedCallback );
        Thread.sleep(GENERAL_TIMEOUT);
        reset( recordChangedCallback );
        reset( recordPathChangedCallback );
    }

    /**
     * Has
     */
    @Given("^the client checks if the server has the record \"([^\"]*)\"$")
    public void the_client_checks_if_the_server_has_the_record(final String recordName) throws InterruptedException {
        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    hasRecord = client.record.has(recordName);
                } catch (DeepstreamError deepstreamError) {
                    hasRecordError = deepstreamError;
                }
            }
        }).start();

        Thread.sleep(GENERAL_TIMEOUT);
    }

    @Then("^the client is not told if the record \"([^\"]*)\" exists$")
    public void the_client_is_not_told_if_the_record_exists(String recordName) throws Throwable {
        //since it is sync this will never happen
    }

    @Then("^the client is told the record \"([^\"]*)\" doesn't exist$")
    public void the_client_is_told_the_record_doesn_t_exist(String recordName) throws Throwable {
        Assert.assertFalse(hasRecord);
    }

    @Then("^the client is told the record \"([^\"]*)\" exists$")
    public void the_client_is_told_the_record_exists(String recordName) throws Throwable {
        Assert.assertTrue(hasRecord);
    }

    @Then("^the client is told the record \"([^\"]*)\" encountered an error checking if record exists$")
    public void the_client_is_told_the_record_encountered_an_error_checking_if_record_exists(String recordName) throws Throwable {
        Assert.assertNotNull(hasRecordError);
    }

    /**
     * Snapshot
     */

    @Given("^the client requests a snapshot for the record \"([^\"]*)\"$")
    public void the_client_requests_a_snapshot_for_the_record(final String recordName) throws Throwable {
        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    snapshotData = client.record.snapshot(recordName);
                } catch (DeepstreamError deepstreamError) {
                    snapshotError = deepstreamError;
                }
            }
        }).start();

        Thread.sleep(GENERAL_TIMEOUT);
    }

    @Then("^the client has no response for the snapshot of record \"([^\"]*)\"$")
    public void the_client_has_no_response_for_the_snapshot_of_record(String recordName) throws Throwable {
        //since it is sync this will never happen
    }

    @Then("^the client is told the record \"([^\"]*)\" encountered an error retrieving snapshot$")
    public void the_client_is_told_the_record_encountered_an_error_retrieving_snapshot(String recordName) throws Throwable {
        Assert.assertNotNull(snapshotError);
    }

    @Then("^the client is provided the snapshot for record \"([^\"]*)\" with data \"(.*)\"$$")
    public void the_client_is_provided_the_snapshot_name_for_record_with_data(String recordName, String data) throws Throwable {
        Assert.assertEquals(gson.fromJson(data, JsonElement.class), snapshotData);
    }

    /**
     * Conflict
     */
    @When("^the client selects \"REMOTE_WINS\" merge strategy for record \"(.*)\"$")
    public void the_client_selects_REMOTE_WINS_merge_strategy(String recordName) throws Throwable {
        record.setMergeStrategy(MergeStrategy.REMOTE_WINS);
    }

    @When("^the client selects \"LOCAL_WINS\" merge strategy for record \"(.*)\"$")
    public void the_client_selects_LOCAL_WINS_merge_strategy(String recordName) throws Throwable {
        record.setMergeStrategy(MergeStrategy.LOCAL_WINS);
    }

    @When("^the client selects \"CUSTOM\" merge strategy for record \"([^\"]*)\"$")
    public void the_client_selects_merge_strategy_for_record(String recordName) throws Throwable {
        record.setMergeStrategy(new RecordMergeStrategy() {
            @Override
            public JsonElement merge(Record record, JsonElement remoteValue, int remoteVersion) throws RecordMergeStrategyException {
                remoteValue.getAsJsonObject().addProperty( "key", "customValue" );
                return remoteValue;
            }
        });
    }


}
