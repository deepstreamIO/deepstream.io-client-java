package io.deepstream;


import io.deepstream.constants.Event;
import io.deepstream.constants.RecordEvents;
import io.deepstream.constants.Topic;

import java.util.HashMap;
import java.util.Map;

public class RecordHandler {

    private IConnection connection;
    private IDeepstreamClient client;
    private Map<String, Record> records;

    public RecordHandler(IConnection connection, IDeepstreamClient client) {
        this.connection = connection;
        this.client = client;
        this.records = new HashMap<>();
    }


    public Record getRecord( String name ) {
        Record record = records.get( name );
        if( record == null ) {
            record = new Record( name, new HashMap(), connection, new HashMap(), client );
            records.put( name, record );
            setListeners( record );
        }
        record.usages++;
        return record;
    }

    private void setListeners(Record record) {
        record.on(RecordEvents.ERROR, new Emitter.Listener() {
            public void call(Object... args) {
                String[] data = (String[]) args;
                client.onError(Topic.RECORD, Event.getEvent(data[ 1 ]), data[ 2 ] );
            }
        });
        record.on(RecordEvents.DESTROY_PENDING, new Emitter.Listener() {
            public void call(Object... args) {

            }
        });
        record.on(RecordEvents.DELETED, new Emitter.Listener() {
            public void call(Object... args) {

            }
        });
        record.on(RecordEvents.DISCARD, new Emitter.Listener() {
            public void call(Object... args) {

            }
        });
    }
}
