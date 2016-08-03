package io.deepstream;

/**
 * A listener that notifies the user whenever the record state is ready. Listeners can be added via
 * {@link Record#addRecordReadyListener(RecordReadyListener)} and removed via
 * {@link Record#removeRecordReadyListener(RecordReadyListener)}
 */
public interface RecordReadyListener {
    /**
     * Called when the record is loaded from the server
     * @param recordName The name of the record which is now ready
     * @param record The record which is now ready / loaded from server
     */
    void onRecordReady( String recordName, Record record );
}
