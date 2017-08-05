package io.deepstream;

public class HasResult {

    boolean exists;
    DeepstreamError error = null;

    HasResult(boolean exists, DeepstreamError error) {
        this.exists = exists;
        this.error = error;
    }

    public boolean hasError() {
        return error != null;
    }

    public DeepstreamError getError() {
        return this.error;
    }

    public boolean getResult() {
        return this.exists;
    }
}
