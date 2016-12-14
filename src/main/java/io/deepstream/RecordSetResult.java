package io.deepstream;


/**
 * When setting a record with write acknowledgement {@link Record#setWithAck(String, Object)} (String)} where the
 * result is either a RecordSetResult containing an error or null.
 */
public class RecordSetResult {

    final private String error;

    public RecordSetResult(String error) {
        this.error = error;
    }

    public String getResult() {
        return error;
    }
}
