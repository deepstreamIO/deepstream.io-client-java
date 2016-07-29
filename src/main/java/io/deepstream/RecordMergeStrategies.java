package io.deepstream;

import com.google.gson.JsonElement;
import io.deepstream.constants.MergeStrategy;

import java.util.HashMap;
import java.util.Map;

class RecordMergeStrategies {
    static final RecordMergeStrategies INSTANCE = new RecordMergeStrategies();
    private final Map<MergeStrategy, RecordMergeStrategy> strategies;

    public RecordMergeStrategies() {
        strategies = new HashMap();
        strategies.put(MergeStrategy.REMOTE_WINS, new RecordMergeStrategy() {
            @Override
            public JsonElement merge(Record record, JsonElement remoteValue, int remoteVersion) {
                return remoteValue;
            }
        });
        strategies.put(MergeStrategy.LOCAL_WINS, new RecordMergeStrategy() {
            @Override
            public JsonElement merge(Record record, JsonElement remoteValue, int remoteVersion) {
                return record.get();
            }
        });
    }

    public RecordMergeStrategy getMergeStrategy(MergeStrategy mergeStrategy) {
        return strategies.get(mergeStrategy);
    }
}
