package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import com.google.gson.JsonElement;

/**
 * Record data changed listener, used to be notified whenever the record data has been modified either locally or remotely.
 */
public interface RecordChangedCallback {
    /**
     * Called when the listener is added via {@link Record#subscribe(RecordChangedCallback, boolean)}<br/>
     * Will contain the entire record data, regardless of whether triggered by a Patch or Update
     * @param recordName The name of the record change
     * @param data The entire data as a {@link JsonElement}, can be retrieved via {@link JsonElement#getAsJsonObject()}
     */
    @ObjectiveCName("onRecordChanged:data:")
    void onRecordChanged(String recordName, JsonElement data );
}
