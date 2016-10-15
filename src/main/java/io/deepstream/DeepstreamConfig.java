package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import java.util.Properties;

class DeepstreamConfig {
    final private Properties properties;

    DeepstreamConfig() {
        properties = new Properties();
    }

    @ObjectiveCName("init:")
    DeepstreamConfig( Properties properties ) throws InvalidDeepstreamConfig {
        this.properties = properties;

        try {
            this.getPath();
            this.getEndpointType();
            this.getReconnectIntervalIncrement();
            this.getMaxReconnectAttempts();
            this.getRpcAckTimeout();
            this.getRpcResponseTimeout();
            this.getSubscriptionTimeout();
            this.getMaxReconnectAttempts();
            this.getTimeBetweenSendingQueuedPackages();
            this.getRecordReadAckTimeout();
            this.getRecordReadTimeout();
            this.getRecordDeleteTimeout();
        } catch( Exception e ) {
            throw new InvalidDeepstreamConfig();
        }

    }

    String getPath() {
        return getOption(ConfigOptions.PATH, "/deepstream");
    }

    EndpointType getEndpointType() {
        return EndpointType.getEndpointType(getOption(ConfigOptions.ENDPOINT_TYPE, EndpointType.WEBSOCKET.toString()));
    }

    int getReconnectIntervalIncrement() {
        return Integer.parseInt(getOption(ConfigOptions.RECONNECT_INTERVAL_INCREMENT, "4000"));
    }

    int getMaxReconnectInterval() {
        return Integer.parseInt(getOption(ConfigOptions.MAX_RECONNECT_INTERVAL, "1500"));
    }

    int getMaxReconnectAttempts() {
        return Integer.parseInt(getOption(ConfigOptions.MAX_RECONNECT_ATTEMPTS, "5"));
    }

    int getRpcAckTimeout() {
        return Integer.parseInt(getOption(ConfigOptions.RPC_ACK_TIMEOUT, "6000"));
    }

    int getRpcResponseTimeout() {
        return Integer.parseInt(getOption(ConfigOptions.RPC_RESPONSE_TIMEOUT, "10000"));
    }

    int getSubscriptionTimeout() {
        return Integer.parseInt(getOption(ConfigOptions.SUBSCRIPTION_TIMEOUT, "2000"));
    }

    int getMaxMessagesPerPacket() {
        return Integer.parseInt(getOption(ConfigOptions.MAX_MESSAGES_PER_PACKET, "100"));
    }

    int getTimeBetweenSendingQueuedPackages() {
        return Integer.parseInt(getOption(ConfigOptions.TIME_BETWEEN_SENDING_QUEUED_PACKAGES, "16"));
    }

    int getRecordReadAckTimeout() {
        return Integer.parseInt(getOption(ConfigOptions.RECORD_READ_ACK_TIMEOUT, "1000"));
    }

    int getRecordReadTimeout() {
        return Integer.parseInt(getOption(ConfigOptions.RECORD_READ_TIMEOUT, "3000"));
    }

    int getRecordDeleteTimeout() {
        return Integer.parseInt(getOption(ConfigOptions.RECORD_DELETE_TIMEOUT, "3000"));
    }

    @ObjectiveCName("getOption:defaultValue:")
    private String getOption(ConfigOptions option, String defaultValue) {
        if (properties.containsKey(option)) {
            return properties.get(option).toString();
        } else if (properties.containsKey(option.toString())) {
            return properties.get(option.toString()).toString();
        } else {
            return defaultValue;
        }
    }

}
