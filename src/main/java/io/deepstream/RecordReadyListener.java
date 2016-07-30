package io.deepstream;


public interface RecordReadyListener {
    void onRecordReady( String recordName, Record record );
}
