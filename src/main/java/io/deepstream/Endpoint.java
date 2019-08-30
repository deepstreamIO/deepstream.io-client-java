package io.deepstream;
import com.google.protobuf.MessageLite;

/**
 * The interface required for any connection endpoints. Currently we support websockets
 */
interface Endpoint {
    /**
     * Message to send to the deepstream server
     * @param message The message to send (TOPIC|ACTION|ARRAY+)
     */
    void send(MessageLite message);

    /**
     * Close the connection
     */
    void close();

    /**
     * Open the connection
     */
    void open();

    /**
     * Forces the connection to be closed
     */
    void forceClose();
}
