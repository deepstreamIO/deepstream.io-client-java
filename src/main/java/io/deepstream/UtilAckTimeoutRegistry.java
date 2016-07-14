package io.deepstream;

import io.deepstream.constants.Actions;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.util.*;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.TimerTask;

class UtilAckTimeoutRegistry implements ConnectionChangeListener {

    private Map<String, AckTimeoutTask> register;
    private Timer timer;
    private Topic topic;
    private long timeoutDuration;
    private IDeepstreamClient client;
    private ConnectionState state;
    private LinkedBlockingQueue<AckTimeoutTask> ackTimers;

    /**
     * The registry for all ack timeouts.
     *
     * @param client The client it sends errors to
     * @param topic The topic for all acks in the register
     * @param timeoutDuration The timeout duration for acks in milliseconds
     */
    public UtilAckTimeoutRegistry(IDeepstreamClient client, Topic topic, long timeoutDuration ) {
        this.client = client;
        this.register = new HashMap<String, AckTimeoutTask>();
        this.ackTimers = new LinkedBlockingQueue<AckTimeoutTask>();
        this.timer = new Timer();
        this.topic = topic;
        this.timeoutDuration = timeoutDuration;

        this.state = client.getConnectionState();
        this.client.addConnectionChangeListener( this );
    }

    /**
     * Clears the ack timeout for a message.
     *
     * @param message The message received to remove the ack timer for
     */
    public void clear( Message message ) {
        String uniqueName = this.getUniqueName( Actions.getAction( message.data[ 0 ] ), message.data[ 1 ] );
        if( this.clear( uniqueName ) == false ) {
            this.client.onError( this.topic, Event.UNSOLICITED_MESSAGE, message.raw );
        }
    }

    /**
     * Checks to see if an ack timer already exists in the register for
     * the given name and action. If it does, it clears it, then starts a new one.
     *
     * @param name The name to be added to the register
     * @param action The action to be added to the register
     */
    public void add( String name, Actions action ) {
        String uniqueName = this.getUniqueName( action, name );
        AckTimeoutTask task = this.register.get( uniqueName );
        if( task != null ) {
            clear( uniqueName );
        }
        addToRegister( uniqueName );
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
        AckTimeoutTask task = register.get( uniqueName );
        if( task != null ) {
            task.cancel();
            return true;
        } else {
            return false;
        }
    }

    /**
     * Adds the uniqueName to the register. Only schedules the timer if
     * the connection state is OPEN, otherwise it adds to the queue of waiting acks.
     *
     * @param uniqueName The name to be added to the register
     */
    private void addToRegister( String uniqueName ) {
        AckTimeoutTask task = new AckTimeoutTask( uniqueName, this );
        register.put( uniqueName, task );

        if( this.state == ConnectionState.OPEN ) {
            timer.schedule( task, this.timeoutDuration );
        } else {
            this.ackTimers.add( task );
        }
    }

    private void onTimeout( String name ) {
        this.register.remove( name );
        String msg = "No ACK message received in time for " + name;
        this.client.onError( this.topic, Event.ACK_TIMEOUT, msg );
    }

    private void scheduleAcks() {
        AckTimeoutTask task = null;
        while( this.ackTimers.peek() != null ) {
            try {
                task = this.ackTimers.take();
            } catch (InterruptedException e) {
                System.out.println( e );
            }

            if( task != null ) {
                this.timer.schedule( task, this.timeoutDuration );
            }
            task = null;
        }
    }

    private String getUniqueName( Actions action, String name ) {
        if( action != null ) {
            return action.toString() + name;
        } else {
            return name;
        }
    }

    private class AckTimeoutTask extends TimerTask {
        private String name;
        private UtilAckTimeoutRegistry callback;

        AckTimeoutTask( String name, UtilAckTimeoutRegistry callback ) {
            this.name = name;
            this.callback = callback;
        }

        @Override
        public void run() {
            this.callback.onTimeout( this.name );
        }
    }
}