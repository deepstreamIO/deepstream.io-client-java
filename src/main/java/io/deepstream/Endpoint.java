package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

/**
 * The interface required for any connection endpoints. Currently we support {@link EndpointType#WEBSOCKET}
 * and {@link EndpointType#TCP} but will be dropping TCP in upcoming releases.
 */
interface Endpoint {
    /**
     * Message to send to the deepstream server
     * @param message The message to send (TOPIC|ACTION|ARRAY+)
     */
    @ObjectiveCName("send:")
    void send(String message);

    /**
     * Close the connection
     */
    void close();

    /**
     * Open the connection
     */
    void open();
}
