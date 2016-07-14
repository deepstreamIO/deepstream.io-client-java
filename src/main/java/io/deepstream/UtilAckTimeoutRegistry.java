package io.deepstream;

import io.deepstream.constants.Actions;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.*;

class UtilAckTimeoutRegistry implements ConnectionChangeListener {

    private Map<String, ScheduledFuture> register;
    private ScheduledExecutorService executor;
    private IDeepstreamClient client;
    private ConnectionState state;
    private LinkedBlockingQueue<AckTimeout> ackTimers;

    static UtilAckTimeoutRegistry utilAckTimeoutRegistry;
    static UtilAckTimeoutRegistry getAckTimeoutRegistry( IDeepstreamClient client ) {
        if(  utilAckTimeoutRegistry == null ) {
            utilAckTimeoutRegistry = new UtilAckTimeoutRegistry( client );
        }
        return utilAckTimeoutRegistry;
    }

    static void resetAckTimeoutRegistry( IDeepstreamClient client ) {
        utilAckTimeoutRegistry = new UtilAckTimeoutRegistry( client );
    }

    /**
     * The registry for all ack timeouts.
     *
     * @param client The client it sends errors to
     */
    public UtilAckTimeoutRegistry(IDeepstreamClient client) {
        this.client = client;
        this.register = new HashMap<String, ScheduledFuture>();
        this.ackTimers = new LinkedBlockingQueue<AckTimeout>();
        this.executor = Executors.newSingleThreadScheduledExecutor();

        this.state = client.getConnectionState();
        this.client.addConnectionChangeListener( this );
    }

    /**
     * Clears the ack timeout for a message.
     *
     * @param message The message received to remove the ack timer for
     */
    public void clear( Message message ) {
        Actions action;
        String name;
        if( message.action == Actions.ACK ) {
            action = Actions.getAction( message.data[ 0 ] );
            name = message.data[ 1 ];
        } else {
            action = message.action;
            name = message.data[ 0 ];
        }

        String uniqueName = this.getUniqueName( message.topic, action, name );
        if( this.clear( uniqueName ) == false ) {
            this.client.onError( message.topic, Event.UNSOLICITED_MESSAGE, message.raw );
        }
    }

    /**
     * Clears the ack timeout for a message.

     */
    public void clear(  Topic topic, Actions action, String name ) {
        String uniqueName = this.getUniqueName( topic, action, name );
        this.clear( uniqueName );
    }

    /**
     * Checks to see if an ack timer already exists in the register for
     * the given name and action. If it does, it clears it, then starts a new one.
     *
     * @param name The name to be added to the register
     * @param action The action to be added to the register
     */
    public void add( Topic topic, Actions action, String name, int timeout ) {
        this.add( topic, action, name, Event.ACK_TIMEOUT, timeout );
    }

    /**
     * Checks to see if an ack timer already exists in the register for
     * the given name and action. If it does, it clears it, then starts a new one.
     *
     * @param name The name to be added to the register
     * @param action The action to be added to the register
     */
    public void add( Topic topic, Actions action, String name, Event event, int timeout ) {
        String uniqueName = this.getUniqueName( topic, action, name );
        this.clear( uniqueName );

        addToRegister( topic, action, name, event, timeout );
    }

    @Override
    public void connectionStateChanged(ConnectionState connectionState) {
        if( connectionState == ConnectionState.OPEN ) {
            scheduleAcks();
        }
        this.state = connectionState;
    }

    /**
     * Clears the ack timeout for a message.
     *
     * @param uniqueName The name of the message ( and possible action ) to remove the timeout for
     */
    private boolean clear( String uniqueName ) {
        ScheduledFuture scheduledFuture = register.get( uniqueName );
        if( scheduledFuture != null ) {
            scheduledFuture.cancel( false );
            return true;
        } else {
            return false;
        }
    }

    /**
     * Adds the uniqueName to the register. Only schedules the timer if
     * the connection state is OPEN, otherwise it adds to the queue of waiting acks.
     */
    private void addToRegister( Topic topic, Actions action, String name, Event event, int timeoutDuration ) {
        AckTimeout task = new AckTimeout( topic, action, name, event, timeoutDuration, this );

        if( this.state == ConnectionState.OPEN ) {
            ScheduledFuture scheduledFuture = executor.schedule( task, timeoutDuration, TimeUnit.MICROSECONDS);

            String uniqueName = this.getUniqueName( topic, action, name );
            register.put( uniqueName, scheduledFuture );
        } else {
            this.ackTimers.add( task );
        }
    }

    private void onTimeout( AckTimeout ackTimeout ) {
        String uniqueName = this.getUniqueName( ackTimeout.topic, ackTimeout.action, ackTimeout.name );
        this.register.remove( uniqueName );
        String msg = "No ACK message received in time for " + ackTimeout.action + ackTimeout.name;
        this.client.onError( ackTimeout.topic, ackTimeout.event, msg );
    }

    private void scheduleAcks() {
        AckTimeout task = null;
        while( this.ackTimers.peek() != null ) {
            try {
                task = this.ackTimers.take();
            } catch (InterruptedException e) {
                System.out.println( e );
            }

            if( task != null ) {
                this.executor.schedule( task, task.timeout, TimeUnit.MICROSECONDS );
            }
            task = null;
        }
    }

    private String getUniqueName( Topic topic, Actions action, String name ) {
        return topic.toString() + action.toString() + name;
    }

    private class AckTimeout implements Runnable {
        private Topic topic;
        private Actions action;
        private String name;
        private Event event;
        private int timeout;

        private UtilAckTimeoutRegistry callback;

        AckTimeout( Topic topic, Actions action, String name,  Event event, int timeout, UtilAckTimeoutRegistry callback ) {
            this.topic = topic;
            this.action = action;
            this.name = name;
            this.event = event;
            this.timeout = timeout;
            this.callback = callback;
        }

        @Override
        public void run() {
            this.callback.onTimeout( this );
        }
    }
}