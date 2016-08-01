package io.deepstream;

/**
 * The interface required for any connection endpoints. Currently we support {@link io.deepstream.constants.EndpointType#ENGINEIO}
 * and {@link io.deepstream.constants.EndpointType#TCP}. Adding a custom endpoint would require you to fork the repo and can't
 * be injected.
 */
interface Endpoint {
    /**
     * Message to send to the deepstream server
     * @param message The message to send (TOPIC|ACTION|ARRAY+)
     */
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
