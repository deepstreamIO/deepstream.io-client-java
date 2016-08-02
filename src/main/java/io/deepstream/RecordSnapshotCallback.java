package io.deepstream;

import com.google.gson.JsonElement;

/**
 * Callback for getting the record data, used by {@link RecordHandler#snapshot(String, RecordSnapshotCallback)}
 */
public interface RecordSnapshotCallback {
    /**
     * Called when the record data has been loaded
     * @param recordName The name of the record that has been found
     * @param recordData The record data
     */
    void onRecordSnapshot( String recordName, JsonElement recordData );

    /**
     * Called if an error occured while getting the record data. The most likely error case is {@link io.deepstream.constants.Event#RECORD_NOT_FOUND}
     * if the record doesn't exist, followed by {@link io.deepstream.constants.Event#MESSAGE_DENIED}, although
     * it could also be a storage failure such as {@link io.deepstream.constants.Event#STORAGE_RETRIEVAL_TIMEOUT} or
     * {@link io.deepstream.constants.Event#CACHE_RETRIEVAL_TIMEOUT}
     * @param recordName The name of the record that has been found
     * @param deepstreamException The {@link DeepstreamException} thrown
     */
    void onRecordSnapshotError( String recordName, DeepstreamException deepstreamException );
}
