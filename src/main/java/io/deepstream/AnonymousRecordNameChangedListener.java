package io.deepstream;

/**
 * A listener that notifies the user whenever {@link AnonymousRecord#setName(String)} is called
 */
public interface AnonymousRecordNameChangedListener {
    /**
     * Callback containing the new record name and the associated anonymousRecord
     * @param recordName The new recordName
     * @param anonymousRecord The anonymousRecord which name changed
     */
    void recordNameChanged(String recordName, AnonymousRecord anonymousRecord );
}
