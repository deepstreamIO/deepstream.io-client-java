package io.deepstream;

import io.deepstream.constants.Event;

public interface RecordEventsListener {
    void onError(String recordName, Event errorType, String errorMessage); //TODO: Do we only throw exceptions on error?
    void onDestroyPending( String recordName );
    void onRecordDeleted( String recordName );
    void onRecordDiscarded( String recordName );
}
