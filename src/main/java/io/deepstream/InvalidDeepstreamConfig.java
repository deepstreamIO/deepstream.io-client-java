package io.deepstream;

/**
 * Thrown if any options passed into {@link DeepstreamClient(String,java.util.Properties)} are invalid, due to type
 * or invalid enum.
 */
public class InvalidDeepstreamConfig extends Exception {
    InvalidDeepstreamConfig() {
        super();
    }
}
