package io.deepstream;

import com.google.gson.JsonElement;

public interface RecordMergeStrategy {
    JsonElement merge(Record record, JsonElement remoteValue, int remoteVersion ) throws RecordMergeStrategyException;
}
