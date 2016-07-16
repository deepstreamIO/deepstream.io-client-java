package io.deepstream;

import io.deepstream.constants.Actions;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.util.Map;
import java.util.concurrent.*;

class UtilAckTimeoutRegistry implements ConnectionChangeListener, TimeoutListener {

    private Map<String, ScheduledFuture> register;
    private ScheduledExecutorService executor;
    private IDeepstreamClient client;
    private ConnectionState state;
    private LinkedBlockingQueue<AckTimeout> ackTimers;

    /**
     * The registry for all ack timeouts.
     *
     * @param client The client it sends errors to
     */
    public UtilAckTimeoutRegistry(IDeepstreamClient client) {
        this.client = client;
        this.register = new ConcurrentHashMap<String, ScheduledFuture>();
        this.ackTimers = new LinkedBlockingQueue<AckTimeout>();
        this.executor = new ErrorReportingThreadPoolExecutor(1);

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
    public void add( Topic topic, Actions action, String name, Event event, int timeout ) {
        this.add( topic, action, name, event, this, timeout );
    }

    /**
     * Checks to see if an ack timer already exists in the register for
     * the given name and action. If it does, it clears it, then starts a new one.
     *
     * @param name The name to be added to the register
     * @param action The action to be added to the register
     */
    public void add( Topic topic, Actions action, String name, int timeout ) {
        this.add( topic, action, name, Event.ACK_TIMEOUT, this, timeout );
    }

    /**
     * Checks to see if an ack timer already exists in the register for
     * the given name and action. If it does, it clears it, then starts a new one.
     *
     * @param name The name to be added to the register
     * @param action The action to be added to the register
     */
    public void add( Topic topic, Actions action, String name, Event event, TimeoutListener timeoutListener, int timeout ) {
        String uniqueName = this.getUniqueName( topic, action, name );
        this.clear( uniqueName );

        addToRegister( topic, action, name, event, timeoutListener, timeout );
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
    private void addToRegister( Topic topic, Actions action, String name, Event event, TimeoutListener timeoutListener, int timeoutDuration ) {
        AckTimeout task = new AckTimeout( topic, action, name, event, timeoutListener, timeoutDuration );

        if( this.state == ConnectionState.OPEN ) {
            ScheduledFuture scheduledFuture = executor.schedule( task, timeoutDuration, TimeUnit.MILLISECONDS );

            String uniqueName = this.getUniqueName( topic, action, name );
            register.put( uniqueName, scheduledFuture );
        } else {
            System.out.println( "Buffering timeout " + this.state );
            this.ackTimers.add( task );
        }
    }

    @Override
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
        private TimeoutListener timeoutListener;
        private Topic topic;
        private Actions action;
        private String name;
        private Event event;
        private int timeout;

        AckTimeout( Topic topic, Actions action, String name,  Event event, TimeoutListener timeoutListener, int timeout ) {
            this.topic = topic;
            this.action = action;
            this.name = name;
            this.event = event;
            this.timeoutListener = timeoutListener;
            this.timeout = timeout;

            System.out.println( "Creating timeout for " + topic + " " + action + " " + name + " " + event + " " + timeout );
        }

        @Override
        public void run() {
            timeoutListener.onTimeout( topic, action, event, name );
            String msg = "No ACK message received in time for " + action.name() + " " + name;
            System.out.println( "Running timeout" );
            client.onError( topic, event, msg );
        }
    }


    private class ErrorReportingThreadPoolExecutor extends ScheduledThreadPoolExecutor {

        public ErrorReportingThreadPoolExecutor(int corePoolSize) {
            super(corePoolSize);
        }

        @Override
        protected void afterExecute(Runnable r, Throwable t) {
            super.afterExecute(r, t);

            if (t == null && r instanceof Future<?>) {
                try {
                    Object result = ((Future<?>) r).get();
                } catch (CancellationException ce) {
                    //t = ce;
                } catch (ExecutionException ee) {
                    t = ee.getCause();
                } catch (InterruptedException ie) {
                    //Thread.currentThread().interrupt();
                }
            }
            if (t != null) {
                System.out.println( "Timeout executed " +  t );
            }
        }
    }
}