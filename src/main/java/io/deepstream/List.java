package io.deepstream;

import com.google.gson.JsonElement;
import io.deepstream.constants.Event;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

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

        this.recordListeners = new List.RecordListeners( this, this.record );
        this.listReadyListeners = new ArrayList<>();
        this.listChangedListeners = new ArrayList<>();

        this.name = name;

        this.refreshInheritedState();
    }

    public List addRecordEventsListener( RecordEventsListener recordEventsListener ) {
        this.record.addRecordEventsListener( recordEventsListener );
        return this;
    }

    public List removeRecordReadyListener(RecordEventsListener recordEventsListener) {
        this.record.removeRecordEventsListener( recordEventsListener );
        return this;
    }


    public List addListReadyListener( ListReadyListener listReadyListener ) {
        this.listReadyListeners.add( listReadyListener );
        return this;
    }

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
     *
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
     * Removes an entry from the list if it resides at
     * a specific index
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
     * Removes an entry from the list
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
     * Proxies the underlying Record's subscibe method
     * @param listChangedListener
     * @param triggerNow
     * @return
     */
    public List subscribe(ListChangedListener listChangedListener) {
        return this.subscribe( listChangedListener, false );
    }

    /**
     * Proxies the underlying Record's subscibe method
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
     * Proxies the underlying Record's unsubscribe method
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
                for( Integer index : newIndexes ) {
                    if( oldIndexes.size() == 0 ) {
                        for( ListChangedListener listChangedListener : this.listChangedListeners ) {
                            listChangedListener.onEntryAdded( this.name, entryName, index );
                        }
                    } else if( oldIndexes.size() <= index || !oldIndexes.get( index ).equals( newIndexes.get( index ) ) ) {
                        for( ListChangedListener listChangedListener : this.listChangedListeners ) {
                            listChangedListener.onEntryMoved( this.name, entryName, index );
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

    private void refreshInheritedState() {
        this.isReady = this.record.isReady;
        this.isDestroyed = this.record.isDestroyed;
    }

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
