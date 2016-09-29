package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import com.google.gson.JsonElement;

/**
 * Thrown when a version conflict occurs, and is only exposed to the client
 * via {@link RecordEventsListener#onError(String, Event, String)}
 */
public class RecordMergeStrategyException extends RuntimeException {

    /**
     * The new data attempted to be set from the server
     */
    public final JsonElement newData;
    /**
     * The version of the record on the server
     */
    public final int remoteVersion;
    /**
     * The old data currently on the client
     */
    public final JsonElement oldData;
    /**
     * The version of the record on the client
     */
    public final int localVersion;
    /**
     * The error message associated with merge conflict returned by a custom {@link RecordMergeStrategy}
     */
    public final String error;

    /**
     * Use when you don't need any merge conflict data
     * @see RecordMergeStrategyException#RecordMergeStrategyException(int, JsonElement, int, JsonElement, String)
     */
    public RecordMergeStrategyException() {
        this(null);
    }

    /**
     * Use when you don't need a reference to the actual conflicting data
     * @see RecordMergeStrategyException#RecordMergeStrategyException(int, JsonElement, int, JsonElement, String)
     */
    @ObjectiveCName("init:")
    public RecordMergeStrategyException( String error ) {
        this(-1, null, -1, null, error);
    }

    /**
     * An exception that can contain all the merge issues
     * @param localVersion The local version during the merge
     * @param oldData The local data during the merge
     * @param remoteVersion The remote version during the merge
     * @param remoteData The remote data during the merge
     * @param error An error message describing the issue
     */
    @ObjectiveCName("init:oldData:remoteVersion:remoteData:error:")
    public RecordMergeStrategyException(int localVersion, JsonElement oldData, int remoteVersion, JsonElement remoteData, String error ) {
        this.localVersion = localVersion;
        this.oldData = oldData;
        this.remoteVersion = remoteVersion;
        this.newData = remoteData;
        this.error = error;
    }
}
