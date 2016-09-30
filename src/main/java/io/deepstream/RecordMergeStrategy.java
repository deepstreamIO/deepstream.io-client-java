package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import com.google.gson.JsonElement;

/**
 * Allows users to reconcile record versions if the data is not correctly in sync ( out of sync record versions )
 */
public interface RecordMergeStrategy {
    //TODO: Run this in a thread since its blocking?
    /**
     * Whenever a version conflict occurs the MergeStrategy set via {@link Record#setMergeStrategy(RecordMergeStrategy)}
     * will be called to merge the data and send the data back to the server.<br/>
     *
     * This is mainly used for scenarios such as when working on very collaborative records where messages cross on the
     * wire, or for connection drops where the client still updates records in an offline mode.<br/>
     *
     * Throw an error if the merge fails, but keep in mind that this only means it will postpone the merge conflict
     * until the next remote/local update.
     *
     * @param record The {@link Record}, used to retrieve the local {@link Record#version} and data via {@link Record#get()}
     * @param remoteValue The remote value on the server
     * @param remoteVersion The remote version on the server, used to find out if the remote is ahead of the local
     * @return The merged value
     * @throws RecordMergeStrategyException Thrown if a merge conflict occurs, mainly if any dependent logic doesn't work.
     *
     */
    @ObjectiveCName("merge:remoteValue:remoteVersion:")
    JsonElement merge(Record record, JsonElement remoteValue, int remoteVersion ) throws RecordMergeStrategyException;
}
