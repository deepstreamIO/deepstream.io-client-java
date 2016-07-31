package io.deepstream;

/**
 * A DeepstreamLoginException occurs if you login to a connection that was forcefully closed due to {@link io.deepstream.constants.Event#UnauthenticatiedConnectionTimeout}
 * or if the login attempts exceed the maximum {@link io.deepstream.constants.Event#TOO_MANY_AUTH_ATTEMPTS}
 */
public class DeepstreamLoginException extends Exception {
}
