package io.deepstream;

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
    void onRecordChanged(String recordName, JsonElement data );

    /**
     * TODO: Bite the bullet and do it as JsonElement?
     * Called when the listener is added via {@link Record#subscribe(String, RecordChangedCallback, boolean)}<br/>
     * Will contain the data under the path, regardless of whether triggered by a Patch or Update
     * @param recordName The name of the record change
     * @param path The path subscribed to
     * @param data The data under the path as an Object
     */
    void onRecordChanged(String recordName, String path, Object data);
}
