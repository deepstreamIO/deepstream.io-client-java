package io.deepstream;

/**
 * Callback for checking if a record exists locally or remotely, used by {@link RecordHandler#has(String, RecordHasCallback)}
 */
public interface RecordHasCallback {
    /**
     * Called if the record exists either locally ( in which case sync ) or remotely ( async )
     * @param recordName The name of the record that has been found
     */
    void onRecordFound( String recordName );

    /**
     * Called if the record doesn't exist locally and remotely ( always async )
     * @param recordName The name of the record that has been found
     */
    void onRecordNotFound( String recordName );

    /**
     * Called if an error occured while checking for the record remotely. This isn't likely
     * to happen, and if it does will most likely be a {@link io.deepstream.constants.Event#MESSAGE_DENIED}, although
     * it could also be a storage failure such as {@link io.deepstream.constants.Event#STORAGE_RETRIEVAL_TIMEOUT} or
     * {@link io.deepstream.constants.Event#CACHE_RETRIEVAL_TIMEOUT}
     * @param recordName The name of the record that has been found
     * @param deepstreamException The {@link DeepstreamException} thrown
     */
    void onRecordHasError(String recordName, DeepstreamException deepstreamException );
}
