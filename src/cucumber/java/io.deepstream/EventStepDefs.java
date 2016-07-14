package io.deepstream;

import cucumber.api.java.en.Then;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;


public class EventStepDefs {DeepstreamClient client;
    int serverPort;
    int server2Port;

    public EventStepDefs( Context context ) {
        this.client = context.client;
        this.serverPort = context.serverPort;
        this.server2Port = context.server2port;
    }

    Emitter.Listener callback = mock( Emitter.Listener.class );

    @Then("^the client subscribes to an event named \"(.*?)\"$")
    public void the_client_subscribes_to_event( String eventName ) throws InterruptedException {
        client.event.subscribe( eventName, callback );
        Thread.sleep(500);
    }

    @Then("^the client unsubscribes from an event named \"(.*?)\"$")
    public void the_client_unsubscribes_from_event( String eventName ) throws InterruptedException {
        client.event.unsubscribe( eventName, callback );
        Thread.sleep(500);
    }

    @Then("^the client publishes an event named \"(.*?)\" with data \"(.*?)\"$")
    public void client_publishes_event( String eventName, String data ) throws InterruptedException {
        client.event.emit( eventName, data );
        Thread.sleep(500);
    }

    @Then("^the client received the event \"(.*?)\" with data \"(.*?)\"$")
    public void client_receives_event( String rpcName, String data ) throws InterruptedException {
        verify( callback ).call( data );

    }

    @Then("^the client listens to events matching \"(.*?)\"$")
    public void client_listens_to_events_matching( String regex ) throws InterruptedException {
        client.event.listen( regex, callback );
        Thread.sleep(500);
    }

    @Then("^the client unlistens to events matching \"(.*?)\"$")
    public void client_unlistens_to_events_matching( String regex ) throws InterruptedException {
        client.event.unlisten( regex );
        Thread.sleep(500);
    }

    @Then("^the client will be notified of new event match \"(.*?)\"$")
    public void client_notified_of_event_match( String eventName ) throws InterruptedException {
        verify( callback ).call( eventName, true );
    }

    @Then("^the client will be notified of event match removal \"([^\"]*)\"$")
    public void the_client_will_be_notified_of_event_match_removal(String eventName) throws Throwable {
        verify( callback ).call( eventName, false );
    }
}