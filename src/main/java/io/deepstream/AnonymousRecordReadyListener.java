package io.deepstream;

/**
 * A listener that notifies the user whenever the record state is ready. Listeners can be added via
 * {@link AnonymousRecord#addRecordReadyListener(AnonymousRecordReadyListener)} and removed via
 * {@link AnonymousRecord#removeRecordReadyListener(AnonymousRecordReadyListener)}
 */
public interface AnonymousRecordReadyListener {
    /**
     * Called when the underlying record is ready
     * @param recordName The name of the anonymousRecord which is now ready
     * @param anonymousRecord The anonymousRecord which is now ready
     */
    void onRecordReady( String recordName, AnonymousRecord anonymousRecord );
}
