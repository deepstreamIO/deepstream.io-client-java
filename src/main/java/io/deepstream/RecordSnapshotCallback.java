package io.deepstream;

import com.google.gson.JsonElement;

public interface RecordSnapshotCallback {
    void onRecordSnapshot( String recordName, JsonElement recordData );
    void onRecordSnapshotError( String recordName, DeepstreamException deepstreamException );
}
