package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import com.google.gson.JsonElement;

/**
 * Record data changed listener, used to be notified whenever the record data under a path has been modified either locally or remotely.
 */
public interface RecordPathChangedCallback {
    /**
     * Called when the listener is added via {@link Record#subscribe(String, RecordPathChangedCallback, boolean)}<br/>
     * Will contain the data under the path, regardless of whether triggered by a Patch or Update
     * @param recordName The name of the record change
     * @param path The path subscribed to
     * @param data The data under the path as an Object
     */
    @ObjectiveCName("onRecordPathChanged:path:data:")
    void onRecordPathChanged(String recordName, String path, JsonElement data);
}
