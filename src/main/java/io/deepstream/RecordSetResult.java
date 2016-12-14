package io.deepstream;


public class RecordSetResult {

    final private String error;

    public RecordSetResult(String error) {
        this.error = error;
    }

    public String getResult() {
        return error;
    }
}
