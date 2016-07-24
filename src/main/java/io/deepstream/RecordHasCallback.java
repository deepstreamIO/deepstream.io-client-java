package io.deepstream;

public interface RecordHasCallback {
    void onRecordFound( String recordName );
    void onRecordNotFound( String recordName );
    void onRecordError( String recordName, DeepstreamException deepstreamException );
}
