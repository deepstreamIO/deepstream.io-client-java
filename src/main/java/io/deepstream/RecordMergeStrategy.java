package io.deepstream;

public interface RecordMergeStrategy {
    void merge( Record record, Object remoteValue, int remoteVersion, RecordMergeCallback callback );
}
