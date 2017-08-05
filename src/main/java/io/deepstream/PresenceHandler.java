package io.deepstream;


import com.google.j2objc.annotations.ObjectiveCName;

import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

public class PresenceHandler {

    private final int subscriptionTimeout;
    private final UtilEmitter emitter;
    private final DeepstreamConfig deepstreamConfig;
    private final IConnection connection;
    private final DeepstreamClientAbstract client;
    private final UtilAckTimeoutRegistry ackTimeoutRegistry;
    private final UtilSingleNotifier notifier;
    private final ExecutorService executor;

    PresenceHandler(DeepstreamConfig deepstreamConfig, final IConnection connection, DeepstreamClientAbstract client, ExecutorService executor) {
        this.subscriptionTimeout = deepstreamConfig.getSubscriptionTimeout();
        this.connection = connection;
        this.client = client;
        this.emitter = new UtilEmitter();
        this.deepstreamConfig = deepstreamConfig;
        this.ackTimeoutRegistry = client.getAckTimeoutRegistry();
        this.notifier = new UtilSingleNotifier(client, connection, Topic.PRESENCE, Actions.QUERY, subscriptionTimeout);
        this.executor = executor;

        new UtilResubscribeNotifier(this.client, new UtilResubscribeNotifier.UtilResubscribeListener() {
            @Override
            public void resubscribe() {
                if( emitter.listeners(Topic.PRESENCE.toString()).size() != 0 ) {
                    connection.sendMsg(Topic.PRESENCE, Actions.SUBSCRIBE, new String[]{Actions.SUBSCRIBE.toString()});
                }
            }
        });
    }

    /**
     * Queries synchronously for clients logged into deepstream
     *
     * This will block your calling thread
     *
     * @return List<String> a list of currently connected clients
     * @throws DeepstreamError
     */
    public String[] getAll() throws DeepstreamError{
        try {
            return getAllAsync(null).get();
        }catch(ExecutionException e){
            Throwable t = e.getCause();
            if(t instanceof DeepstreamError){
                throw (DeepstreamError)t;
            }else{
                e.printStackTrace();
                return null;
            }
        }catch(Exception e){
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Queries asynchronously for clients logged into deepstream. You can block calling thread by executing .get() on result.
     *
     * @return Future<List<String>> a list of currently connected clients
     * @param listener Callback to be called after query is successfull, may be null
     * @throws DeepstreamError
     */
    public Future<String[]> getAllAsync(final PresenceGetAllResultListener listener) {
        return executor.submit(new Callable<String[]>() {
            @Override
            public String[] call() throws Exception {
                final Object[] data = new Object[1];
                final DeepstreamError[] deepstreamException = new DeepstreamError[1];

                final CountDownLatch snapshotLatch = new CountDownLatch(1);

                notifier.request(Actions.QUERY.toString(), new UtilSingleNotifier.UtilSingleNotifierCallback() {
                    @Override
                    public void onSingleNotifierError(String name, DeepstreamError error) {
                        deepstreamException[0] = error;
                        snapshotLatch.countDown();
                    }

                    @Override
                    public void onSingleNotifierResponse(String name, Object users) {
                        data[0] = users;
                        snapshotLatch.countDown();
                    }
                });

                try {
                    snapshotLatch.await();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }

                if (deepstreamException[0] != null) {
                    throw deepstreamException[0];
                }
                if(listener != null){
                    listener.getAllCompleted((String[]) data[0]);
                }
                return (String[]) data[0];
            }
        });
    }

    /**
     * Subscribes to clients login/logout events
     *
     * @param eventListener The listener that will be called with the username of the client
     *                      and a boolean to indicated whether they logged in or out
     */
    @ObjectiveCName("subscribe:")
    public void subscribe( PresenceEventListener eventListener ) {
        if (this.emitter.hasListeners(Topic.PRESENCE.toString())) {
            this.ackTimeoutRegistry.add( Topic.PRESENCE, Actions.SUBSCRIBE, Topic.PRESENCE.toString(), this.subscriptionTimeout );
            this.connection.send(MessageBuilder.getMsg(Topic.PRESENCE, Actions.SUBSCRIBE, Actions.SUBSCRIBE.toString()));
        }
        this.emitter.on(Topic.PRESENCE, eventListener);
    }

    /**
     * Removes the listener added via {@link PresenceHandler}
     *
     * @param eventListener The listener that will be called with the username of the client
     *                      and a boolean to indicated whether they logged in or out
     */
    @ObjectiveCName("unsubscribe:")
    public void unsubscribe( PresenceEventListener eventListener ) {
        this.emitter.off(Topic.PRESENCE.toString(), eventListener);
        if (this.emitter.hasListeners(Topic.PRESENCE.toString())) {
            this.ackTimeoutRegistry.add( Topic.PRESENCE,  Actions.UNSUBSCRIBE, Topic.PRESENCE.toString(), this.subscriptionTimeout );
            this.connection.send(MessageBuilder.getMsg(Topic.PRESENCE, Actions.UNSUBSCRIBE, Actions.UNSUBSCRIBE.toString()));
        }
    }


    protected void handle( Message message ) {
        if( message.action == Actions.ERROR && message.data[0].equals(Event.MESSAGE_DENIED.toString()) ) {
            this.ackTimeoutRegistry.clear( message );
            this.client.onError( Topic.PRESENCE, Event.MESSAGE_DENIED, message.data[1] );
        }
        else if( message.action == Actions.ACK ) {
            this.ackTimeoutRegistry.clear( message );
        }
        else if( message.action == Actions.PRESENCE_JOIN ) {
            this.broadcastEvent( Topic.PRESENCE.toString(), message.data[0], true );
        }
        else if( message.action == Actions.PRESENCE_LEAVE ) {
            this.broadcastEvent( Topic.PRESENCE.toString(), message.data[0], false );
        }
        else if( message.action == Actions.QUERY ) {
            this.notifier.recieve(Actions.QUERY.toString(), null, message.data);
        }
        else {
            this.client.onError( Topic.PRESENCE, Event.UNSOLICITED_MESSAGE, message.action.toString() );
        }
    }

    private void broadcastEvent( String eventName, Object... args ) {
        java.util.List<Object> listeners = this.emitter.listeners( eventName );
        for( Object listener : listeners ) {
            if( args != null ) {
                if ((boolean) args[1])
                    ((PresenceEventListener) listener).onClientLogin((String) args[0]);
                else
                    ((PresenceEventListener) listener).onClientLogout((String) args[0]);
            }
        }
    }
}
