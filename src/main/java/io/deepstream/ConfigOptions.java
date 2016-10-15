package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import java.util.Properties;

/**
 * These are all the options available to configure a deepstream client. You can either use them to construct
 * a map to pass into {@link DeepstreamClient#DeepstreamClient(String, Properties)}, or via a {@link Properties} file
 */
public enum ConfigOptions {
    /**
     * Default path to use when connection using websockets
     * Defaults to /deepstream
     */
    PATH("path"),
    /**
     * A {@link EndpointType}, currently either TCP or Websockets
     * Defaults to Websocket
     */
    ENDPOINT_TYPE("endpointType"),
    /**
     * Specifies the number of milliseconds by which the time until the next reconnection attempt will be incremented
     * after every unsuccessful attempt.
     * E.g.for 1500: if the connection is lost,the client will attempt to reconnect immediately, if that fails it will
     * try again after 1.5 seconds, if that fails it will try again after 3 seconds and so on...
     */
    RECONNECT_INTERVAL_INCREMENT("reconnectIntervalIncrement"),
    /**
     * The maximum amount of time that can pass before a reconnect is attempted. This supersedes the
     * multiplying factor provided by RECONNECT_INTERVAL_INCREMENT
     */
    MAX_RECONNECT_INTERVAL("maxReconnectInterval"),
    /**
     * The number of reconnection attempts until the client gives up and declares the connection closed.
     */
    MAX_RECONNECT_ATTEMPTS("maxReconnectAttempts"),
    /**
     * The number of milliseconds after which a RPC will error if no Ack-message has been received.
     */
    RPC_ACK_TIMEOUT("rpcAckTimeout"),
    /**
     * The number of milliseconds after which a RPC will error if no response-message has been received.
     */
    RPC_RESPONSE_TIMEOUT("rpcResponseTimeout"),
    /**
     * The number of milliseconds that can pass after providing/unproviding a RPC or subscribing/unsubscribing/listening
     * to a record or event before an error is thrown.
     */
    SUBSCRIPTION_TIMEOUT("subscriptionTimeout"),
    /**
     * If your app sends a large number of messages in quick succession, the deepstream client will try to split them
     * into smaller packets and send these every ms. This parameter specifies the number of messages after which
     * deepstream sends the packet and queues the remaining messages.
     */
    MAX_MESSAGES_PER_PACKET("maxMessagesPerPacket"),
    /**
     * Please see description for maxMessagesPerPacket. Sets the time in ms.
     */
    TIME_BETWEEN_SENDING_QUEUED_PACKAGES("timeBetweenSendingQueuedPackages"),
    /**
     * The number of milliseconds from the moment client.record.getRecord() is called until an error is thrown since no
     * ack message has been received.
     */
    RECORD_READ_ACK_TIMEOUT("recordReadAckTimeout"),
    /**
     * The number of milliseconds from the moment client.record.getRecord() is called until an error is thrown since no
     * data has been received.
     */
    RECORD_READ_TIMEOUT("recordReadTimeout"),
    /**
     * The number of milliseconds from the moment record.delete() is called until an error is thrown since no delete ack
     * message has been received. Please take into account that the deletion is only complete after the record has been deleted from both cache and storage.
     */
    RECORD_DELETE_TIMEOUT("recordDeleteTimeout");

    private String configOption;

    @ObjectiveCName("init:")
    ConfigOptions(String topic) {
        this.configOption = topic;
    }

    @Override
    public String toString() {
        return this.configOption;
    }
}