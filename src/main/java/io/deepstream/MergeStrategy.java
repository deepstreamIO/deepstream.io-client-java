package io.deepstream;

/**
 * Provides all the built in merge strategies that are supported
 */
public enum MergeStrategy {
    /**
     * This automatically accepts the remote data as the source of truth
     */
    REMOTE_WINS,
    /**
     * This automatically accepts the local data as the source of truth
     */
    LOCAL_WINS
}

