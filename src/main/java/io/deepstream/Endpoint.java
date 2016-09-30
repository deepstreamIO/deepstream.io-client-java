package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

/**
 * The interface required for any connection endpoints. Currently we support {@link EndpointType#ENGINEIO}
 * and {@link EndpointType#TCP}. Adding a custom endpoint would require you to fork the repo and can't
 * be injected.
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
