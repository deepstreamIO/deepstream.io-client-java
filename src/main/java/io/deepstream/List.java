package io.deepstream;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import io.deepstream.constants.Event;

import java.util.ArrayList;
import java.util.Map;

public class List implements RecordEventsListener, RecordReadyListener {
    private final Record record;
    private final RecordHandler recordHandler;
    private final String name;

    /**
     * A List is a specialised Record that contains
     * an Array of recordNames and provides a number
     * of convinience methods for interacting with them.
     *
     * @param recordHandler
     * @param name
     * @param options
     *
     * @constructor
     */
    public List(RecordHandler recordHandler, String name, Map options) {
        this.recordHandler = recordHandler;
        this.record = this.recordHandler.getRecord( name );
        this.record.addRecordEventsListener( this );

        this.name = name;
    }

    /**
     * Returns the array of list entries or an
     * empty array if the list hasn't been populated yet.
     * @return
     */
    public java.util.List getEntries() {
        java.util.List<String> entries;
        try {
            entries = (java.util.List<String>) this.record.get();
        } catch( IllegalStateException ex ) {
            entries = new ArrayList<>();
        }
        return entries;
    }

    /**
     *
     * @param entries
     * @return
     */
    public List setEntries( JsonArray entries ) {
        if( this.record.isReady == false ) {
            // Buffer, why?
        }
        else {
            this.beforeChange();
            this.record.set( entries );
            this.afterChange();
        }
        return this;
    }

    /**
     * Removes an entry from the list if it resides at
     * a specific index
     * @param entry
     * @param index
     * @return
     */
    public List removeEntry( String entry ) {
        return this.removeEntry( entry, -1 );
    }

    /**
     * Removes an entry from the list
     * @param entry
     * @param index
     * @return
     */
    public List removeEntry( String entry, int index ) {
        if( this.record.isReady == false ) {
            // Buffer, why?
        }
        else {
        }

        return this;
    }

    /**
     * Returns true if the list is empty
     * @return
     */
    public boolean isEmpty() {
        return this.getEntries().size() == 0;
    }

    private void afterChange() {
    }

    private void beforeChange() {
    }

    @Override
    public void onError(String recordName, Event errorType, String errorMessage) {

    }

    @Override
    public void onDestroyPending(String recordName) {

    }

    @Override
    public void onRecordDeleted(String recordName) {

    }

    @Override
    public void onRecordDiscarded(String recordName) {

    }

    @Override
    public void onRecordReady(String recordName, Record record) {

    }
}
