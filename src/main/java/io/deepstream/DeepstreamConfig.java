package io.deepstream;

import io.deepstream.constants.ConfigOptions;
import io.deepstream.constants.EndpointType;

import java.util.Properties;

class DeepstreamConfig {
    private Properties properties;

    DeepstreamConfig() {
    }

    DeepstreamConfig( Properties properties ) throws InvalidDeepstreamConfig {
        this.properties = properties;

        try {
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
            this.getObjectDeltas();
        } catch( Exception e ) {
            throw new InvalidDeepstreamConfig();
        }

    }

    EndpointType getEndpointType() {
        return EndpointType.getEndpointType( properties.getProperty( ConfigOptions.ENDPOINT_TYPE.toString(), EndpointType.TCP.toString() ) );
    }

    int getReconnectIntervalIncrement() {
        return Integer.parseInt( properties.getProperty( ConfigOptions.RECONNECT_INTERVAL_INCREMENT.toString(), "4000" ) );
    }

    int getMaxReconnectInterval() {
        return Integer.parseInt( properties.getProperty( ConfigOptions.MAX_RECONNECT_INTERVAL.toString(), "1500" ) );
    }

    int getMaxReconnectAttempts() {
        return Integer.parseInt( properties.getProperty( ConfigOptions.MAX_RECONNECT_ATTEMPTS.toString(), "5" ) );
    }

    int getRpcAckTimeout() {
        return Integer.parseInt( properties.getProperty( ConfigOptions.RPC_ACK_TIMEOUT.toString(), "6000" ) );
    }

    int getRpcResponseTimeout() {
        return Integer.parseInt( properties.getProperty( ConfigOptions.RPC_RESPONSE_TIMEOUT.toString(), "10000" ) );
    }

    int getSubscriptionTimeout() {
        return Integer.parseInt( properties.getProperty( ConfigOptions.SUBSCRIPTION_TIMEOUT.toString(), "2000" ) );
    }

    int getMaxMessagesPerPacket() {
        return Integer.parseInt( properties.getProperty( ConfigOptions.MAX_MESSAGES_PER_PACKET.toString(), "100" ) );
    }

    int getTimeBetweenSendingQueuedPackages() {
        return Integer.parseInt( properties.getProperty( ConfigOptions.MAX_MESSAGES_PER_PACKET.toString(), "16" ) );
    }

    int getRecordReadAckTimeout() {
        return Integer.parseInt( properties.getProperty( ConfigOptions.RECORD_READ_ACK_TIMEOUT.toString(), "1000" ) );
    }

    int getRecordReadTimeout() {
        return Integer.parseInt( properties.getProperty( ConfigOptions.RECORD_READ_TIMEOUT.toString(), "3000" ) );
    }

    int getRecordDeleteTimeout() {
        return Integer.parseInt( properties.getProperty( ConfigOptions.RECORD_DELETE_TIMEOUT.toString(), "3000" ) );
    }

    boolean getObjectDeltas() {
        return Boolean.parseBoolean( properties.getProperty( ConfigOptions.OBJECT_DELTAS.toString(), "false" ) );
    }
}
