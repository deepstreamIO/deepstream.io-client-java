package io.deepstream;

interface Endpoint {
    void send(String message);
    void close();
    void open();
}
