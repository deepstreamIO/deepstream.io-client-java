package io.deepstream;

public enum ConnectionState {
    CLOSED,
    AWAITING_CONNECTION,
    CHALLENGING,
    AWAITING_AUTHENTICATION,
    AUTHENTICATING,
    OPEN,
    ERROR,
    RECONNECTING
}
