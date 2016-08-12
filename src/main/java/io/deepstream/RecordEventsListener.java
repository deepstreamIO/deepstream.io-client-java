package io.deepstream;

/**
 * Record state changed listener, used to be notified whenever the record state has occurred
 */
public interface RecordEventsListener {
    /**
     * Notified whenever an error has occurred on the record, usually due to an async operation such as a timeout
     * or VersionConflict that can't be caught sync.
     *
     * @param recordName The name of the record an error occured to
     * @param errorType The error type, such as {@link Event#ACK_TIMEOUT} or {@link Event#MESSAGE_DENIED}
     * @param errorMessage An error message in english, describing the issue. Do not use this message other than for
     *                     logging! All checks should be against errorType
     */
    void onError(String recordName, Event errorType, String errorMessage);

    /**
     * Notified when the record was deleted, whether by this client or by another.<br/>
     * Once this is called the record object must be cleaned up and a new one created if you wish
     * to continue setting data.
     * @param recordName The name of the deleted record
     */
    void onRecordDeleted( String recordName );

    /**
     * Notified once the record was discarded.<br/>
     * Once this is called the record object must be cleaned up and a new one created if you wish
     * to continue setting data.
     * @param recordName The name of the discarded record
     */
    void onRecordDiscarded(String recordName);
}
