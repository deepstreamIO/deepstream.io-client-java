package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

/**
 * A listener that notifies the user whenever the record state is ready.
 */
@ObjectiveCName("RecordReadyListener")
public interface RecordReadyListener {
    /**
     * Called when the record is loaded from the server
     *
     * @param recordName The name of the record which is now ready
     * @param record     The record which is now ready / loaded from server
     */
    @ObjectiveCName("onRecordReady:record:")
    void onRecordReady(String recordName, Record record);
}
