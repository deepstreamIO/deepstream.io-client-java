package io.deepstream;

import com.google.gson.JsonElement;

public interface RecordChangedCallback {
    void onRecordChanged(String recordName, JsonElement data );
    void onRecordChanged(String recordName, String path, Object data);
}
