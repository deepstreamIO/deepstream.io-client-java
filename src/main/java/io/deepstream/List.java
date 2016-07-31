package io.deepstream;

import com.google.gson.JsonElement;
import io.deepstream.constants.Event;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * A List is a specialised Record that contains
 * an Array of recordNames and provides a number
 * of convinience methods for interacting with them.
 */
public class List {
    public boolean isReady;
    public boolean isDestroyed;
    public final String name;

    private final RecordListeners recordListeners;
    private final Record record;
    private final RecordHandler recordHandler;
    private final ArrayList<ListReadyListener> listReadyListeners;
    private final ArrayList<ListChangedListener> listChangedListeners;

    /**
     * Constructor is not public since it is created via {@link RecordHandler#getList(String)}
     */
    List(RecordHandler recordHandler, String name, Map options) {
        this.recordHandler = recordHandler;
        this.record = this.recordHandler.getRecord( name );

        this.recordListeners = new List.RecordListeners( this, this.record );
        this.listReadyListeners = new ArrayList<>();
        this.listChangedListeners = new ArrayList<>();

        this.name = name;

        this.refreshInheritedState();
    }

    /**
     * Adds a Listener that will notify you if a Discard, Delete or Error event occurs
     * @param recordEventsListener
     * @return
     */
    public List addRecordEventsListener( RecordEventsListener recordEventsListener ) {
        this.record.addRecordEventsListener( recordEventsListener );
        return this;
    }

    /**
     * Remove listener added via {@link List#addRecordEventsListener(RecordEventsListener)}
     * @param recordEventsListener
     * @return
     */
    public List removeRecordEventsListener(RecordEventsListener recordEventsListener) {
        this.record.removeRecordEventsListener( recordEventsListener );
        return this;
    }


    /**
     * Add listener to be notified when the List has been loaded from the server
     * @param listReadyListener
     * @return
     */
    public List addListReadyListener( ListReadyListener listReadyListener ) {
        this.listReadyListeners.add( listReadyListener );
        return this;
    }

    /**
     * Remove listener added via {@link List#addListReadyListener(ListReadyListener)}
     * @param listReadyListener
     * @return
     */
    public List removeListReadyListener(ListReadyListener listReadyListener) {
        this.listReadyListeners.remove( listReadyListener );
        return this;
    }

    /**
     * Returns the array of list entries or an
     * empty array if the list hasn't been populated yet.
     * @return
     */
    public java.util.List getEntries() {
        ArrayList<String> entries;
        try {
            entries = this.record.get( ArrayList.class );
        } catch( Exception ex ) {
            entries = new ArrayList<>();
        }
        return entries;
    }

    /**
     * Updates the list with a new set of entries
     * @param entries
     * @return
     */
    public List setEntries( java.util.List<String> entries ) {
        if( this.record.isReady == false ) {
            //TODO: Buffer ( to disable events from being emitted )
        }
        else {
            this.updateList( entries );
        }
        return this;
    }

    /**
     * Removes the first occurrence of an entry from the list
     * @param entry
     * @return
     */
    public List removeEntry( String entry ) {
        if( !this.isReady ) {
            //TODO: Buffer ( to disable events from being emitted )
            return this;
        }

        Collection entries = this.getEntries();
        while( entries.contains( entry ) ) entries.remove( entry );
        this.updateList( entries );
        return this;
    }

    /**
     * Removes an entry from the list if it resides at
     * a specific index
     * @param entry
     * @param index
     * @return
     */
    public List removeEntry( String entry, int index ) {
        if( !this.isReady ) {
            //TODO: Buffer ( to disable events from being emitted )
            return this;
        }

        java.util.List entries = this.getEntries();
        if( entries.get( index ).equals( entry ) ) {
            entries.remove( index );
        }
        this.updateList(entries);

        return this;
    }

    /**
     * Add an entry to the end of the list
     * @param entry
     * @return
     */
    public List addEntry( String entry ) {
        if( !this.isReady ) {
            //TODO: Buffer ( to disable events from being emitted )
            return this;
        }

        java.util.List entries = this.getEntries();
        entries.add( entry );
        this.updateList( entries );
        return this;
    }

    /**
     * Add an entry at a certain index into the list
     * @param entry
     * @param index
     * @return
     */
    public List addEntry( String entry, int index ) {
        if( !this.isReady ) {
            //TODO: Buffer ( to disable events from being emitted )
            return this;
        }

        java.util.List entries = this.getEntries();
        entries.add( index, entry );
        this.updateList( entries );
        return this;
    }

    /**
     * Returns true if the list is empty
     * @return
     */
    public boolean isEmpty() {
        return this.getEntries().size() == 0;
    }

    /**
     * Notifies the user whenever the list has changed
     * @param listChangedListener
     * @return
     */
    public List subscribe(ListChangedListener listChangedListener) {
        return this.subscribe( listChangedListener, false );
    }

    /**
     * Notifies the user whenever the list has changed, and notifies immediately if triggerNow is true
     * @param listChangedListener
     * @param triggerNow
     * @return
     */
    public List subscribe(ListChangedListener listChangedListener, boolean triggerNow ) {
        this.listChangedListeners.add( listChangedListener );

        if( this.listChangedListeners.size() == 1 ) {
            this.record.subscribe( this.recordListeners );
        }

        if( triggerNow ) {
            for( ListChangedListener listChangeListener : this.listChangedListeners ) {
                listChangeListener.onListChanged( this.name, this.getEntries() );
            }
        }

        return this;
    }

    /**
     * Removes the listener added via {@link List#subscribe(ListChangedListener, boolean)}
     * @param listChangedListener
     * @return
     */
    public List unsubscribe(ListChangedListener listChangedListener) {
        this.listChangedListeners.remove(listChangedListener);

        if( this.listChangedListeners.size() == 0 ) {
            this.record.unsubscribe( this.recordListeners );
        }

        return this;
    }

    /**
     * Useful entry point for diffing previous list and new one to get entries added, removed and moved
     * @param entries
     */
    private void updateList(Collection entries) {
        Map oldStructure = this.beforeChange();
        this.record.set( entries );
        this.afterChange( oldStructure );
    }

    /**
     * Establishes the current structure of the list, provided the client has attached any
     * add / move / remove listener
     *
     * This will be called before any change to the list, regardsless if the change was triggered
     * by an incoming message from the server or by the client
     *
     * @return
     */
    private Map beforeChange() {
        if( this.listChangedListeners.isEmpty() ) {
            return null;
        }
        return this.getStructure();
    }

    /**
     * Compares the structure of the list after a change to its previous structure and notifies
     * any add / move / remove listener. Won't do anything if no listeners are attached.
     *
     * @returns
     */
    private void afterChange( Map<String,ArrayList<Integer>> oldStructure ) {
        if( oldStructure == null ) {
            return;
        }
        Map<String, ArrayList<Integer>> newStructure = this.getStructure();


        for( String entryName : oldStructure.keySet() ) {
            ArrayList<Integer> oldIndexes = oldStructure.get( entryName );
            ArrayList<Integer> newIndexes = newStructure.get( entryName );

            for( Integer index : oldIndexes ) {
                if( newIndexes == null ) {
                    for( ListChangedListener listChangedListener : this.listChangedListeners ) {
                        listChangedListener.onEntryRemoved( this.name, entryName, index );
                    }
                }
            }
        }

        for( String entryName : newStructure.keySet() ) {
            ArrayList<Integer> oldIndexes = oldStructure.get( entryName );
            ArrayList<Integer> newIndexes = newStructure.get( entryName );

            if( oldIndexes == null ) {
                for( Integer index : newIndexes ) {
                    for( ListChangedListener listChangedListener : this.listChangedListeners ) {
                        listChangedListener.onEntryAdded( this.name, entryName, index );
                    }
                }
            } else {
                for( int i=0; i<newIndexes.size(); i++ ) {
                    Integer index = newIndexes.get( i );
                    if( oldIndexes.size() < i || !oldIndexes.get( i ).equals( newIndexes.get( i ) ) ) {
                        if( oldIndexes.size() < i ) {
                            for( ListChangedListener listChangedListener : this.listChangedListeners ) {
                                listChangedListener.onEntryAdded( this.name, entryName, index );
                            }
                        } else {
                            for( ListChangedListener listChangedListener : this.listChangedListeners ) {
                                listChangedListener.onEntryMoved( this.name, entryName, index );
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Iterates through the list and creates a map with the entry as a key
     * and an array of its position(s) within the list as a value, e.g.
     *
     * {
     * 	'recordA': [ 0, 3 ],
     * 	'recordB': [ 1 ],
     * 	'recordC': [ 2 ]
     * }
     *
     * @return
     */
    private Map getStructure() {
        Map structure = new HashMap<String, ArrayList>();
        java.util.List<String> entries = this.getEntries();

        for( int i=0; i<entries.size();i++) {
            ArrayList list = (ArrayList) structure.get( entries.get(i) );
            if( list == null ) {
                list = new ArrayList<Integer>();
                structure.put( entries.get( i ), list );
            }
            list.add( i );
        }

        return structure;
    }

    /**
     * Mirror properties on record onto list
     */
    private void refreshInheritedState() {
        this.isReady = this.record.isReady;
        this.isDestroyed = this.record.isDestroyed;
    }

    /**
     * A class to contain all the interface implementations to not pollute the public API
     */
    private class RecordListeners implements RecordReadyListener, RecordChangedCallback, RecordEventsListener, Record.RecordRemoteUpdateListener {

        private final List list;
        private final Record record;
        private Map beforeChange;

        RecordListeners( List list, Record record ) {
            this.list = list;
            this.record = record;
            this.record.addRecordEventsListener( this );
            this.record.addRecordReadyListener( this );
            this.record.setRecordRemoteUpdateListener( this );
        }

        @Override
        public void onError(String recordName, Event errorType, String errorMessage) {
        }

        @Override
        public void onDestroyPending(String recordName) {
            this.list.refreshInheritedState();
        }

        @Override
        public void onRecordDeleted(String recordName) {
            this.list.refreshInheritedState();
        }

        @Override
        public void onRecordDiscarded(String recordName) {
            this.list.refreshInheritedState();
        }

        @Override
        public void onRecordReady(String recordName, Record record) {
            this.list.refreshInheritedState();
            for( ListReadyListener listReadyListener : this.list.listReadyListeners ) {
                listReadyListener.onListReady( this.list.name, this.list );
            }
        }

        @Override
        public void onRecordChanged(String recordName, JsonElement data) {
            for( ListChangedListener listChangeListener : this.list.listChangedListeners ) {
                listChangeListener.onListChanged( this.list.name, this.list.getEntries() );
            }
        }

        @Override
        public void onRecordChanged(String recordName, String path, Object data) {
        }

        @Override
        public void beforeRecordUpdate() {
            this.beforeChange = this.list.beforeChange();
        }

        @Override
        public void afterRecordUpdate() {
            this.list.afterChange( this.beforeChange );
        }

    }
}
