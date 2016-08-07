package io.deepstream.constants;

public enum ConfigOptions {
    ENDPOINT_TYPE("endpointType"),
    RECONNECT_INTERVAL_INCREMENT("reconnectIntervalIncrement"),
    MAX_RECONNECT_INTERVAL("maxReconnectInterval"),
    MAX_RECONNECT_ATTEMPTS("maxReconnectAttempts"),
    RPC_ACK_TIMEOUT("rpcAckTimeout"),
    RPC_RESPONSE_TIMEOUT("rpcResponseTimeout"),
    SUBSCRIPTION_TIMEOUT("subscriptionTimeout"),
    MAX_MESSAGES_PER_PACKET("maxMessagesPerPacket"),
    TIME_BETWEEN_SENDING_QUEUED_PACKAGES("timeBetweenSendingQueuedPackages"),
    RECORD_READ_ACK_TIMEOUT("recordReadAckTimeout"),
    RECORD_READ_TIMEOUT("recordReadTimeout"),
    RECORD_DELETE_TIMEOUT("recordDeleteTimeout"),
    OBJECT_DELTAS("objectDeltas");

    private String configOption;

    ConfigOptions(String topic) {
        this.configOption = topic;
    }

    @Override
    public String toString() {
        return this.configOption;
    }
}