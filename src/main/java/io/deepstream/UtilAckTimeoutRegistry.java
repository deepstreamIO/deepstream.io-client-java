package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import java.util.Map;
import java.util.concurrent.*;

class UtilAckTimeoutRegistry implements ConnectionStateListener, UtilTimeoutListener {

    private final Map<String, ScheduledFuture> register;
    private final ScheduledExecutorService executor;
    private final DeepstreamClientAbstract client;
    private final LinkedBlockingQueue<AckTimeout> ackTimers;

    private ConnectionState state;

    /**
     * The registry for all ack timeouts.
     *
     * @param client The client it sends errors to
     */
    @ObjectiveCName("init:")
    UtilAckTimeoutRegistry(DeepstreamClientAbstract client) {
        this.client = client;
        this.register = new ConcurrentHashMap<>();
        this.ackTimers = new LinkedBlockingQueue<>();
        this.executor = new ScheduledThreadPoolExecutor(5);

        this.state = client.getConnectionState();
        this.client.addConnectionChangeListener( this );
    }

    /**
     * Clears the ack timeout for a message.
     *
     * @param message The message received to remove the ack timer for
     */
    @ObjectiveCName("clear:")
    void clear( Message message ) {
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
        if(!this.clear(uniqueName)) {
            this.client.onError( message.topic, Event.UNSOLICITED_MESSAGE, message.raw );
        }
    }

    /**
     * Clears the ack timeout for a message.
     *
     * @param name The name to be added to the register
     * @param action The action to be added to the register
     */
    @ObjectiveCName("clear:action:name:")
    void clear(  Topic topic, Actions action, String name ) {
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
    @ObjectiveCName("add:action:name:event:timeout:")
    void add( Topic topic, Actions action, String name, Event event, int timeout ) {
        this.add( topic, action, name, event, this, timeout );
    }

    /**
     * Checks to see if an ack timer already exists in the register for
     * the given name and action. If it does, it clears it, then starts a new one.
     *
     * @param name The name to be added to the register
     * @param action The action to be added to the register
     */
    @ObjectiveCName("add:action:name:timeout:")
    void add( Topic topic, Actions action, String name, int timeout ) {
        this.add( topic, action, name, Event.ACK_TIMEOUT, this, timeout );
    }

    /**
     * Checks to see if an ack timer already exists in the register for
     * the given name and action. If it does, it clears it, then starts a new one.
     *
     * @param name The name to be added to the register
     * @param action The action to be added to the register
     */
    @ObjectiveCName("add:action:name:event:timeoutListener:timeout:")
    void add(Topic topic, Actions action, String name, Event event, UtilTimeoutListener timeoutListener, int timeout ) {
        String uniqueName = this.getUniqueName( topic, action, name );
        this.clear( uniqueName );

        addToRegister( topic, action, name, event, timeoutListener, timeout );
    }

    @Override
    @ObjectiveCName("connectionStateChanged:")
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
    @ObjectiveCName("clearWithUniqueName:")
    private boolean clear( String uniqueName ) {
        ScheduledFuture scheduledFuture = register.get( uniqueName );
        if( scheduledFuture != null ) {
            scheduledFuture.cancel( false );
            register.remove( uniqueName );
            return true;
        } else {
            return false;
        }
    }

    /**
     * Adds the uniqueName to the register. Only schedules the timer if
     * the connection state is OPEN, otherwise it adds to the queue of waiting acks.
     */
    @ObjectiveCName("addToRegister:action:name:event:timeoutListener:timeoutDuration:")
    private void addToRegister(Topic topic, Actions action, String name, Event event, UtilTimeoutListener timeoutListener, int timeoutDuration ) {
        AckTimeout task = new AckTimeout( topic, action, name, event, timeoutListener, timeoutDuration );

        if( this.state == ConnectionState.OPEN ) {
            ScheduledFuture scheduledFuture = executor.schedule( task, timeoutDuration, TimeUnit.MILLISECONDS );

            String uniqueName = this.getUniqueName( topic, action, name );
            register.put( uniqueName, scheduledFuture );
        } else {
            this.ackTimers.add( task );
        }
    }

    @Override
    @ObjectiveCName("onTimeout:action:event:name:")
    public void onTimeout(Topic topic, Actions action, Event event, String name ) {
        String uniqueName = this.getUniqueName( topic, action, name );
        this.register.remove( uniqueName );
    }

    private void scheduleAcks() {
        AckTimeout task = null;
        while( this.ackTimers.peek() != null ) {
            try {
                task = this.ackTimers.take();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            if( task != null ) {
                this.executor.schedule( task, task.timeout, TimeUnit.MICROSECONDS );
            }
            task = null;
        }
    }

    @ObjectiveCName("getUniqueName:action:name:")
    private String getUniqueName( Topic topic, Actions action, String name ) {
        return topic.toString() + action.toString() + name;
    }

    private class AckTimeout implements Runnable {
        private final UtilTimeoutListener timeoutListener;
        private final Topic topic;
        private final Actions action;
        private final String name;
        private final Event event;
        private final int timeout;

        @ObjectiveCName("init:action:name:event:timeoutListener:timeout:")
        AckTimeout(Topic topic, Actions action, String name, Event event, UtilTimeoutListener timeoutListener, int timeout ) {
            this.topic = topic;
            this.action = action;
            this.name = name;
            this.event = event;
            this.timeoutListener = timeoutListener;
            this.timeout = timeout;
        }

        @Override
        public void run() {
            String msg;

            timeoutListener.onTimeout( topic, action, event, name );
            if( event == Event.ACK_TIMEOUT ) {
               msg = "No ACK message received in time for " + action.name() + " " + name;
            } else {
                msg = "No message received in time for " + action.name() + " " + name;
            }
            client.onError( topic, event, msg );
        }
    }
}