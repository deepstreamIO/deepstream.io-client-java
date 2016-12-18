package io.deepstream;

import com.google.gson.JsonElement;

public class SnapshotResult {

    JsonElement data;
    DeepstreamError error = null;

    SnapshotResult(JsonElement data, DeepstreamError error) {
        this.data = data;
        this.error = error;
    }

    public boolean hasError() {
        return error != null;
    }

    public DeepstreamError getError() {
        return this.error;
    }

    public JsonElement getData() {
        return this.data;
    }
}
