package io.deepstream;

import com.google.gson.JsonElement;
import io.deepstream.constants.Event;

/**
 * Thrown when a version conflict occurs, and is only exposed to the client
 * via {@link RecordEventsListener#onError(String, Event, String)}
 */
public class RecordMergeStrategyException extends RuntimeException {

    public JsonElement newData;
    public int remoteVersion;
    public JsonElement oldData;
    public int localVersion;
    public String error;

    /**
     * Use when you don't need any merge conflict data
     * @see RecordMergeStrategyException#RecordMergeStrategyException(int, JsonElement, int, JsonElement, String)
     */
    public RecordMergeStrategyException() {
    }

    /**
     * Use when you don't need a reference to the actual conflicting data
     * @see RecordMergeStrategyException#RecordMergeStrategyException(int, JsonElement, int, JsonElement, String)
     */
    public RecordMergeStrategyException( String error ) {
        this.error = error;
    }

    /**
     * An exception that can contain all the merge issues
     * @param localVersion The local version during the merge
     * @param oldData The local data during the merge
     * @param remoteVersion The remote version during the merge
     * @param remoteData The remote data during the merge
     * @param error An error message describing the issue
     */
    public RecordMergeStrategyException(int localVersion, JsonElement oldData, int remoteVersion, JsonElement remoteData, String error ) {
        this.localVersion = localVersion;
        this.oldData = oldData;
        this.remoteVersion = remoteVersion;
        this.newData = remoteData;
        this.error = error;
    }
}
