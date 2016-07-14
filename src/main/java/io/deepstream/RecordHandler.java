package io.deepstream;


import io.deepstream.constants.Event;
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
        if( !records.containsKey( name ) ) {
            Record record = new Record( name, new HashMap(), connection, new HashMap(), client );
            records.put( name, record );
            setListeners( record );
        }

        Record record = records.get( name );
        record.usages++;

        return record;
    }

    private void setListeners(Record record) {
        record.on("error", new Emitter.Listener() {
            public void call(Object... args) {
                String[] data = (String[]) args;
                client.onError(Topic.RECORD, Event.getEvent(data[ 1 ]), data[ 2 ] );
            }
        });
        record.on("destroyPending", new Emitter.Listener() {
            public void call(Object... args) {

            }
        });
        record.on("deleted", new Emitter.Listener() {
            public void call(Object... args) {

            }
        });
        record.on("discard", new Emitter.Listener() {
            public void call(Object... args) {

            }
        });
    }
}
