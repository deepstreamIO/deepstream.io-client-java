package io.deepstream;

import com.google.gson.JsonElement;

import java.util.ArrayList;

/**
 * An AnonymousRecord is a record without a predefined name. It
 * acts like a wrapper around an actual record that can
 * be swapped out for another one whilst keeping all bindings intact.
 *
 * Imagine a customer relationship management system with a list of users
 * on the left and a user detail panel on the right. The user detail
 * panel could use the anonymous record to set up its bindings, yet whenever
 * a user is chosen from the list of existing users the anonymous record's
 * setName method is called and the detail panel will update to
 * show the selected user's details
 */
public class AnonymousRecord {

    /**
     * The record name the anonymous record is currently referring to
     */
    public String name;

    private final RecordListeners recordListeners;
    private final ArrayList<Subscription> subscriptions;
    private final ArrayList<AnonymousRecordNameChangedListener> anonymousRecordNameChangedCallbacks;
    private final RecordHandler recordHandler;
    private final ArrayList<AnonymousRecordReadyListener> recordReadyListeners;

    private Record record;

    /**
     * This constructor is called by the {@link RecordHandler#getAnonymousRecord()}
     * @param recordHandler The recordHandler used to get record instances
     */
    AnonymousRecord(RecordHandler recordHandler) {
        this.recordHandler = recordHandler;
        this.subscriptions = new ArrayList<>();
        this.anonymousRecordNameChangedCallbacks = new ArrayList<>();
        this.recordReadyListeners = new ArrayList<>();
        this.recordListeners = new AnonymousRecord.RecordListeners( this );
    }

    /**
     * Add a ready listener to the anonymousRecord. Whenever the record state is you'll get notified via
     * {@link AnonymousRecordReadyListener#onRecordReady(String, AnonymousRecord)}. Note that since you can
     * change the underlying record at will, this method can be called multiple times.
     * @param recordReadyListener The listener to add
     * @return The AnonymousRecord
     */
    public AnonymousRecord addRecordReadyListener( AnonymousRecordReadyListener recordReadyListener ) {
        this.recordReadyListeners.add( recordReadyListener );
        return this;
    }

    /**
     * Remove a readyListener added via {@link AnonymousRecord#addRecordReadyListener(AnonymousRecordReadyListener)}
     *
     * @param recordEventsListener The listener to remove
     * @return The AnonymousRecord
     */
    public AnonymousRecord removeRecordReadyListener(AnonymousRecordReadyListener recordEventsListener) {
        this.recordReadyListeners.remove( recordEventsListener );
        return this;
    }

    /**
     * Add a callback to be notified whenever {@link AnonymousRecord#setName(String)} is called.
     * @param anonymousRecordNameChangedCallback The listener to add
     * @return The AnonymousRecord
     */
    public AnonymousRecord addRecordNameChangedListener(AnonymousRecordNameChangedListener anonymousRecordNameChangedCallback ) {
        this.anonymousRecordNameChangedCallbacks.add( anonymousRecordNameChangedCallback );
        return this;
    }

    /**
     * Remove a callback used to be notified whenever {@link AnonymousRecord#setName(String)} is called.
     * @param anonymousRecordNameChangedCallback The listener to remove
     * @return The AnonymousRecord
     */
    public AnonymousRecord removeRecordNameChangedCallback(AnonymousRecordNameChangedListener anonymousRecordNameChangedCallback ) {
        this.anonymousRecordNameChangedCallbacks.remove( anonymousRecordNameChangedCallback );
        return this;
    }

    /**
     * Proxies to the actual{@link Record#set(Object)} method. It is valid
     * to call get prior to setName - if no record exists,
     * the method returns null
     * @return The AnonymousRecord
     */
    public AnonymousRecord set( Object data ) throws AnonymousRecordUninitialized {
        return this.set( null, data );
    }

    /**
     * Proxies to the actual{@link Record#set(String, Object)} method. It is valid
     * to call get prior to setName - if no record exists,
     * the method returns null
     * @return The AnonymousRecord
     */
    public AnonymousRecord set( String path, Object data ) throws AnonymousRecordUninitialized {
        if( this.record == null ) {
            throw new AnonymousRecordUninitialized( "set" );
        }
        this.record.set(path, data);
        return this;
    }

    /**
     * Proxies to the actual{@link Record#discard()} method. If a record does
     * not exist it will throw a {@link AnonymousRecordUninitialized} exception
     * @return The AnonymousRecord
     */
    public AnonymousRecord discard() throws AnonymousRecordUninitialized {
        if( this.record == null ) {
            throw new AnonymousRecordUninitialized( "discard" );
        }
        this.record.discard();
        return this;
    }

    /**
     * Proxies to the actual {@link Record#delete()} method. If a record does
     * not exist it will throw a {@link AnonymousRecordUninitialized} exception
     * @return The AnonymousRecord
     */
    public AnonymousRecord delete() throws AnonymousRecordUninitialized {
        if( this.record == null ) {
            throw new AnonymousRecordUninitialized( "delete" );
        }
        this.record.delete();
        return this;
    }

    /**
     * Proxies to the actual {@link Record#get()} method. It is valid
     * to call get prior to setName - if no record exists,
     * the method returns null
     * @return The JsonElement
     */
    public JsonElement get() {
        if( this.record == null ) {
            return null;
        }
        return this.record.get();
    }

    /**
     * Proxies to the actual {@link Record#get(String)} method. It is valid
     * to call get prior to setName - if no record exists,
     * the method returns null
     * @param path The path to retrieve
     * @return The JsonElement
     */
    public JsonElement get(String path ) {
        if( this.record == null ) {
            return null;
        }
        return this.record.get( path );
    }

    /**
     * @see AnonymousRecord#subscribe
     * @param recordChangedCallback The listener to add
     * @return The AnonymousRecord
     */
    public AnonymousRecord subscribe( RecordChangedCallback recordChangedCallback ) {
        return this.subscribe( null, recordChangedCallback );
    }

    /**
     * Proxies the actual {@link Record#subscribe} method. The same parameters
     * can be used. Can be called prior to {@link AnonymousRecord#setName}). Please note, triggerIfReady
     * will always be set to true to reflect changes in the underlying record.
     * @param path The path to listen to
     * @param recordChangedCallback The listener to add
     * @return The AnonymousRecord
     */
    public AnonymousRecord subscribe( String path, RecordChangedCallback recordChangedCallback ) {
        this.subscriptions.add( new Subscription( path, recordChangedCallback ) );

        if( this.record != null ) {
            this.record.subscribe( path, recordChangedCallback, true );
        }

        return this;
    }

    /**
     * @see AnonymousRecord#unsubscribe
     * @param recordChangedCallback The listener to remove
     * @return The AnonymousRecord
     */
    public AnonymousRecord unsubscribe( RecordChangedCallback recordChangedCallback ) {
        return this.unsubscribe( null, recordChangedCallback );
    }

    /**
     * Proxies the actual {@link Record#unsubscribe} method. The same parameters
     * can be used. Can be called prior to {@link AnonymousRecord#setName}).
     * @param path The path to unlisten to
     * @param recordChangedCallback The listen to remove
     * @return The AnonymousRecord
     */
    public AnonymousRecord unsubscribe( String path, RecordChangedCallback recordChangedCallback ) {
        this.subscriptions.remove( new Subscription( path, recordChangedCallback ) );

        for( Subscription subscription : subscriptions ) {
            if( subscription.path.equals( path ) && subscription.recordChangedCallback == recordChangedCallback ) {
                subscriptions.remove( subscription );
            }
        }

        if( this.record != null ) {
            this.record.unsubscribe( path, recordChangedCallback );
        }

        return this;
    }

    /**
     * Proxies the actual {@link Record#addRecordEventsListener} method. The same parameters
     * can be used. Can be called prior to {@link AnonymousRecord#setName}).
     * @param recordEventListener The listener to add
     * @return The AnonymousRecord
     */
    public AnonymousRecord addRecordEventsListener(RecordEventsListener recordEventListener ) {
        this.subscriptions.add( new Subscription( recordEventListener ) );

        if( this.record != null ) {
            this.record.addRecordEventsListener( recordEventListener );
        }

        return this;
    }

    /**
     * Proxies the actual {@link Record#removeRecordEventsListener} method. The same parameters
     * can be used. Can be called prior to {@link AnonymousRecord#setName}).
     * @param recordEventListener The listener to remove
     * @return The AnonymousRecord
     */
    public AnonymousRecord removeRecordEventsListener(RecordEventsListener recordEventListener ) {
        for( Subscription subscription : this.subscriptions ) {
            if( subscription.recordChangedCallback != null ) {
                this.subscriptions.remove( subscription );
            }
        }

        if( this.record != null ) {
            this.record.removeRecordEventsListener( recordEventListener );
        }

        return this;
    }

    /**
     * Sets the underlying record the anonymous record is bound
     * to. Can be called multiple times.
     *
     * @param recordName The name of the underlying record to use
     * @return The AnonymousRecord
     */
    public AnonymousRecord setName( String recordName ) {
        this.name = recordName;
        this.unsubscribeRecord();
        this.record = this.recordHandler.getRecord( recordName );
        this.subscribeRecord();

        if( this.record.isReady() ) {
            this.recordListeners.onRecordReady( this.name, this.record );
        }

        for( AnonymousRecordNameChangedListener anonymousRecordNameChangedCallback : this.anonymousRecordNameChangedCallbacks ) {
            anonymousRecordNameChangedCallback.recordNameChanged( recordName, this );
        }

        return this;
    }

    /**
     * Subscribe all callbacks to current record
     */
    private void subscribeRecord() {
        for( Subscription subscription : this.subscriptions ) {
            if( subscription.recordChangedCallback != null ) {
                this.record.subscribe( subscription.path, subscription.recordChangedCallback, true );
            }
        }
        this.record.addRecordReadyListener( this.recordListeners );
    }

    /**
     * Unsubscribe all callbacks from current record
     */
    private void unsubscribeRecord() {
        if( this.record == null || this.record.isDestroyed() ) {
            return;
        }

        for( Subscription subscription : this.subscriptions ) {
            if( subscription.recordChangedCallback != null ) {
                this.record.unsubscribe( subscription.path, subscription.recordChangedCallback );
            }

        }

        this.record.removeRecordReadyListener( this.recordListeners );
        this.record.discard();
    }

    /**
     * A class that contains subscriptions to remove/add when changing the underlying record
     */
    private class Subscription {
        String path;
        RecordChangedCallback recordChangedCallback;
        RecordEventsListener recordEventsListener;
        Subscription(String path, RecordChangedCallback recordChangedCallback ) {
            this.path = path;
            this.recordChangedCallback = recordChangedCallback;
        }
        Subscription(RecordEventsListener recordEventsListener ) {
            this.recordEventsListener = recordEventsListener;
        }
    }

    /**
     * An inner class to avoid having interface methods on the public API
     */
    private class RecordListeners implements RecordReadyListener {
        private final AnonymousRecord anonymousRecord;

        RecordListeners(AnonymousRecord anonymousRecord) {
            this.anonymousRecord = anonymousRecord;
        }

        @Override
        public void onRecordReady(String recordName, Record record) {
            for( AnonymousRecordReadyListener anonymousRecordReadyListener : this.anonymousRecord.recordReadyListeners) {
                anonymousRecordReadyListener.onRecordReady(recordName, this.anonymousRecord );
            }
        }
    }

}
