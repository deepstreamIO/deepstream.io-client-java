package io.deepstream.message;

public interface Endpoint {
    void send(String message);
    void close() throws Exception;
}
