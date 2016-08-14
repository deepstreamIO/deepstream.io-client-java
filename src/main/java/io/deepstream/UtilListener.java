package io.deepstream;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

class UtilListener implements UtilResubscribeNotifier.UtilResubscribeListener {

    private final Topic topic;
    private final DeepstreamConfig deepstreamConfig;
    private final UtilResubscribeNotifier resubscribeNotifier;

    private UtilAckTimeoutRegistry ackTimoutRegistry;
    private String pattern;
    private ListenListener listenerCallback;
    private DeepstreamClientAbstract client;
    private IConnection connection;

    private ExecutorService executorService;

    public UtilListener(Topic topic, String pattern, ListenListener listenerCallback, DeepstreamConfig deepstreamConfig, DeepstreamClientAbstract client, IConnection connection) {
        this.topic = topic;
        this.pattern = pattern;
        this.listenerCallback = listenerCallback;
        this.deepstreamConfig = deepstreamConfig;
        this.client = client;
        this.connection = connection;
        this.resubscribeNotifier = new UtilResubscribeNotifier( this.client, this );
        this.ackTimoutRegistry = client.getAckTimeoutRegistry();
        this.executorService = Executors.newCachedThreadPool();
    }

    /**
     * Send the listen request to the server
     */
    void start() {
        this.scheduleAckTimeout();
        this.sendListen();
    }

    public void destroy() {
        this.connection.sendMsg( this.topic, Actions.UNLISTEN, new String[] { this.pattern } );
        this.resubscribeNotifier.destroy();
        this.listenerCallback = null;
        this.pattern = null;
        this.client = null;
        this.connection = null;
        this.ackTimoutRegistry = null;
    }

    public void onMessage(final Message message) {
        if( message.action.equals( Actions.ACK ) ) {
            this.ackTimoutRegistry.clear( message );
        } else {
            if (message.action.equals(Actions.SUBSCRIPTION_FOR_PATTERN_FOUND)) {
                boolean accepted = listenerCallback.onSubscriptionForPatternAdded(message.data[1]);
                if (accepted) {
                    sendAccept(message.data[1]);
                } else {
                    sendReject(message.data[1]);
                }
            } else if (message.action.equals(Actions.SUBSCRIPTION_FOR_PATTERN_REMOVED)) {
                listenerCallback.onSubscriptionForPatternRemoved(message.data[1]);
            }
        }
    }

    private void sendListen() {
        this.connection.sendMsg( this.topic, Actions.LISTEN, new String[] { this.pattern } );
    }

    private void sendAccept(String subscription) {
        this.connection.sendMsg(this.topic, Actions.LISTEN_ACCEPT, new String[]{this.pattern, subscription});
    }

    private void sendReject(String subscription) {
        this.connection.sendMsg(this.topic, Actions.LISTEN_REJECT, new String[]{this.pattern, subscription});
    }

    private void scheduleAckTimeout() {
        int subscriptionTimeout = deepstreamConfig.getSubscriptionTimeout();
        this.ackTimoutRegistry.add( this.topic, Actions.LISTEN, this.pattern, subscriptionTimeout );
    }

    @Override
    public void resubscribe() {
        this.scheduleAckTimeout();
        sendListen();
    }
}
