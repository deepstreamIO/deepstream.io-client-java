package io.deepstream;

public interface RecordHasCallback {
    void onRecordFound( String recordName );
    void onRecordNotFound( String recordName );
    void onRecordHasError(String recordName, DeepstreamException deepstreamException );
}
