package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

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
    @ObjectiveCName("onError:errorType:errorMessage:")
    void onError(String recordName, Event errorType, String errorMessage);

    /**
     * Notified when a listener has confirmed that it will be publishing data. This is extremely useful
     * in the case of records, where you can indicate whether the values shown are cached or up to date.
     * @param recordName The name of the record which has gained/lost a provider
     * @param hasProvider true if provider was found, false otherwise
     */
    @ObjectiveCName("onRecordHasProviderChanged:hasProvider:")
    void onRecordHasProviderChanged(String recordName, boolean hasProvider);

    /**
     * Notified when the record was deleted, whether by this client or by another.<br/>
     * Once this is called the record object must be cleaned up and a new one created if you wish
     * to continue setting data.
     * @param recordName The name of the deleted record
     */
    @ObjectiveCName("onRecordDeleted:")
    void onRecordDeleted( String recordName );

    /**
     * Notified once the record was discarded.<br/>
     * Once this is called the record object must be cleaned up and a new one created if you wish
     * to continue setting data.
     * @param recordName The name of the discarded record
     */
    @ObjectiveCName("onRecordDiscarded:")
    void onRecordDiscarded(String recordName);
}
