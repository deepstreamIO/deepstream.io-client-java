package io.deepstream;

public interface AnonymousRecordReadyListener {
    void onRecordReady( String recordName, AnonymousRecord record );
}
