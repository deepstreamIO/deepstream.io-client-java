package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import com.google.gson.JsonElement;

import java.util.HashMap;
import java.util.Map;

/**
 * Built in merge strategies
 */
class RecordMergeStrategies {
    static final RecordMergeStrategies INSTANCE = new RecordMergeStrategies();
    private final Map<MergeStrategy, RecordMergeStrategy> strategies;

    public RecordMergeStrategies() {
        strategies = new HashMap<>();
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

    /**
     * Returns a build in merge strategy stored by {@link MergeStrategy}
     * @param mergeStrategy The merge strategy enum defined in {@link MergeStrategy}
     * @return The {@link RecordMergeStrategy}
     */
    @ObjectiveCName("getMergeStrategy:")
    public RecordMergeStrategy getMergeStrategy(MergeStrategy mergeStrategy) {
        return strategies.get(mergeStrategy);
    }
}
