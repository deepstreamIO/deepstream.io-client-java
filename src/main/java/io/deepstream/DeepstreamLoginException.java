package io.deepstream;

/**
 * A DeepstreamLoginException occurs if you login to a connection that was forcefully closed due to {@link Event#UNAUTHENTICATED_CONNECTION_TIMEOUT}
 * or if the login attempts exceed the maximum {@link Event#TOO_MANY_AUTH_ATTEMPTS}
 */
public class DeepstreamLoginException extends Exception {
}
