package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

/**
 * A listener that notifies the user whenever {@link AnonymousRecord#setName(String)} is called
 */
public interface AnonymousRecordNameChangedListener {
    /**
     * Notified whenever the anonymous record changes
     * @param recordName The new recordName
     * @param anonymousRecord The anonymousRecord which name changed
     */
    @ObjectiveCName("recordNameChanged:anonymousRecord:")
    void recordNameChanged(String recordName, AnonymousRecord anonymousRecord );
}
