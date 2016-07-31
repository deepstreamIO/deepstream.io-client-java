package io.deepstream;


import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.deepstream.constants.*;

import java.util.*;
import java.util.List;


/**
 * This class represents a single record - an observable
 * dataset returned by {@link RecordHandler#getRecord(String)}
 */
class Record {

    public int usages;
    public int version;

    public boolean isReady;
    public boolean isDestroyed;

    private static final String ALL_EVENT = "ALL_EVENT";
    private static final String DESTROY_PENDING = "DESTROY_PENDING";

    private final UtilResubscribeNotifier utilResubscribeNotifier;
    private final UtilAckTimeoutRegistry ackTimeoutRegistry;
    private final IConnection connection;
    private final IDeepstreamClient client;
    private final Gson gson;
    private final UtilJSONPath path;
    private final UtilEmitter subscribers;

    private ArrayList<RecordEventsListener> recordEventsListeners;
    private ArrayList<RecordReadyListener> recordReadyListeners;
    private ArrayList<RecordReadyListener> onceRecordReadyListeners;
    private RecordMergeStrategy mergeStrategy;

    private JsonElement data;
    private String name;
    private Map options;
    private RecordRemoteUpdateListener recordRemoteUpdateListener;

    /**
     * Constructor is not public since it is created via {@link RecordHandler#getRecord(String)}
     * @param name The unique name of the record
     * @param recordOptions A map of options, e.g. { persist: true }
     * @param connection The instance of the server connection
     * @param options Deepstream options
     * @param client deepstream.io client
     */
    Record(String name, Map recordOptions, IConnection connection, Map options, IDeepstreamClient client) {
        this.ackTimeoutRegistry = client.getAckTimeoutRegistry();
        this.name = name;
        this.options = options;
        this.usages = 0;
        this.version = -1;
        this.connection = connection;
        this.client = client;
        this.gson = new Gson();
        this.data = new JsonObject();
        this.path = new UtilJSONPath( this.data );
        this.subscribers = new UtilEmitter();
        this.isReady = false;
        this.isDestroyed = false;

        this.recordReadyListeners = new ArrayList<>();
        this.recordEventsListeners = new ArrayList<>();
        this.onceRecordReadyListeners = new ArrayList<>();

        this.scheduleAcks();
        this.sendRead();

        this.utilResubscribeNotifier = new UtilResubscribeNotifier(client, new UtilResubscribeCallback() {
            @Override
            public void resubscribe() {
                sendRead();
            }
        });
    }

    /**
     * Add listener to be notified when the Record has been loaded from the server
     * @param recordReadyListener
     * @return
     */
    public Record addRecordReadyListener( RecordReadyListener recordReadyListener ) {
        this.recordReadyListeners.add( recordReadyListener );
        return this;
    }

    /**
     * Remove listener added via {@link io.deepstream.Record#addRecordReadyListener(RecordReadyListener)}
     * @param recordReadyListener
     * @return
     */
    public Record removeRecordReadyListener(RecordReadyListener recordReadyListener) {
        this.recordEventsListeners.remove( recordReadyListener );
        return this;
    }

    /**
     * Adds a Listener that will notify you if a Discard, Delete or Error event occurs
     * @param recordEventsListener
     * @return
     */
    public Record addRecordEventsListener(RecordEventsListener recordEventsListener) {
        this.recordEventsListeners.add( recordEventsListener );
        return this;
    }

    /**
     * Remove listener added via {@link io.deepstream.Record#addRecordEventsListener(RecordEventsListener)}
     * @param recordEventsListener
     * @return
     */
    public Record removeRecordEventsListener(RecordEventsListener recordEventsListener) {
        this.recordEventsListeners.remove( recordEventsListener );
        return this;
    }

    /**
     * Set a merge strategy that comes with deepstream. These are currently LOCAL_WINS and REMOTE_WINS
     * @param mergeStrategy
     * @return
     */
    public Record setMergeStrategy(MergeStrategy mergeStrategy) {
        this.mergeStrategy = RecordMergeStrategies.INSTANCE.getMergeStrategy( mergeStrategy );
        return this;
    }

    /**
     * Set a custom merge strategy for this record
     * @param mergeStrategy
     * @return
     */
    public Record setMergeStrategy(RecordMergeStrategy mergeStrategy) {
        this.mergeStrategy = mergeStrategy;
        return this;
    }

    /**
     * Gets the a class to represent the record Type.
     * This is currently not public as it is used for the list class, but we
     * should investigate the work done by AlexH to get this to be a more intuitive
     * API
     *
     * @return
     */
    <T> T get( Class<T> type ) {
        return deepCopy( this.data, type );
    }

    /**
     * Gets the value at the path indicated.
     *
     * For example, if the record data is:
     * { "name": "Yasser", pets: [ { type: "Dog", "name": "Whiskey", age: 3} ]}
     *
     * We can do:
     * get( "name" ) -> {@link JsonElement#getAsString()}
     * get( "pets[0]" ) -> {@link JsonElement#getAsJsonObject()}
     * get( "pets[0].age") -> {@link JsonElement#getAsInt()}
     *
     * This has pros and cons. Pro is JSONElement has alot of utility functions, such as
     * {@link JsonElement#getAsBoolean()} and all other primitive types, as well as more advanced
     * types such as {@link JsonElement#getAsJsonArray()}.
     *
     * Con is it ties us quite a bit to the Gson Library
     *
     * @return
     */
    public JsonElement get( String path ) {
        return deepCopy( this.path.get( path ) );
    }

    /**
     * Gets the entire record data and should always return a {@link JsonObject}, except when using
     * a {@link io.deepstream.List}, but then you should always be using it via {@link io.deepstream.List#getEntries()} ;)
     * @return
     */
    public JsonElement get() {
        return deepCopy( this.data );
    }

    /**
     * Set the value for the entire record
     * Make sure that the Object passed in can be serialised to a JsonElement, otherwise it will
     * throw a {@link IllegalStateException}. Best way to guarantee this is by setting Json friendly objects,
     * such as {@link Map}. Since this is a root the object should also not be a primitive.
     * @return
     */
    public Record set( Object value ) throws DeepstreamRecordDestroyedException {
        return this.set( null, value, false );
    }

    /**
     * Set the value for a specific path in your Record data.
     * Make sure that the Object passed in can be serialised to a JsonElement, otherwise it will
     * throw a {@link IllegalStateException}. Best way to guarantee this is by setting Json friendly objects,
     * such as {@link Map}. Since this is at a specific path, you can pass in primitives as long as the path
     * is not null, which is the equivilant of calling {@link Record#set(Object)}.
     * @return
     */
    public Record set(String path, Object value ) throws DeepstreamRecordDestroyedException {
        return this.set( path, value, false );
    }

    /**
     * This forces an update, which is useful when trying to reconcile a merge conflict when the merge is the same
     * but the version number isn't.
     * @param path
     * @param value
     * @param force
     * @return
     * @throws DeepstreamRecordDestroyedException
     */
    private Record set(String path, Object value, boolean force ) throws DeepstreamRecordDestroyedException {
        throwExceptionIfDestroyed( "set" );

        JsonElement element = gson.toJsonTree( value );

        if( !this.isReady ) {
            System.out.println( "Not ready, should queue!" );
            return this;
        }

        JsonElement object = this.path.get( path );

        if( force == false ) {
            if( object != null && object.equals( value ) ) {
                return this;
            } else if( path == null && this.data.equals( value ) ) {
                return this;
            }
        }

        Map oldValues = beginChange();
        this.version++;
        this.path.set( path, element );
        this.data = this.path.getCoreElement();
        sendUpdate( path, value );
        completeChange( oldValues );

        return this;
    }

    /**
     * Notifies the user whenever anything under the path provided has changed.
     * @param path
     * @param recordChangedCallback
     * @return
     * @throws DeepstreamRecordDestroyedException
     */
    public Record subscribe( String path, RecordChangedCallback recordChangedCallback ) throws DeepstreamRecordDestroyedException {
        return subscribe( path, recordChangedCallback, false );
    }

    /**
     * Notifies the user whenever anything inside the Record has changed.
     * @param recordChangedCallback
     * @return
     * @throws DeepstreamRecordDestroyedException
     */
    public Record subscribe( RecordChangedCallback recordChangedCallback ) throws DeepstreamRecordDestroyedException {
        return subscribe( null, recordChangedCallback, false );
    }

    /**
     * Same as {@link Record#subscribe(RecordChangedCallback)}, except if you pass in true for trigger now
     * it will immediately notify the listener if the record has been loaded.
     * @param recordChangedCallback
     * @param triggerNow
     * @return
     * @throws DeepstreamRecordDestroyedException
     */
    public Record subscribe( RecordChangedCallback recordChangedCallback, boolean triggerNow ) throws DeepstreamRecordDestroyedException {
        subscribe( null, recordChangedCallback, triggerNow );
        return this;
    }

    /**
     * Same as {@link Record#subscribe(String,RecordChangedCallback)}, except if you pass in true for trigger now
     * it will immediately notify the listener if the record has been loaded.
     * @param path
     * @param recordChangedCallback
     * @param triggerNow
     * @return
     * @throws DeepstreamRecordDestroyedException
     */
    public Record subscribe( String path, RecordChangedCallback recordChangedCallback, boolean triggerNow ) throws DeepstreamRecordDestroyedException {
        throwExceptionIfDestroyed( "subscribe" );

        if( path == null ) {
            this.subscribers.on( ALL_EVENT, recordChangedCallback );
        } else {
            this.subscribers.on( path, recordChangedCallback );
        }

        if( triggerNow && path == null ) {
            recordChangedCallback.onRecordChanged( this.name, this.get() );
        } else if( triggerNow ) {
            recordChangedCallback.onRecordChanged( this.name, path, this.get( path ) );
        }

        return this;
    }

    /**
     * Remove the listener added via {@link Record#subscribe(RecordChangedCallback)}
     * @param recordChangedCallback
     * @return
     * @throws DeepstreamRecordDestroyedException
     */
    public Record unsubscribe( RecordChangedCallback recordChangedCallback ) throws DeepstreamRecordDestroyedException {
        unsubscribe( null, recordChangedCallback );
        return this;
    }

    /**
     * Remove the listener added via {@link Record#subscribe(String,RecordChangedCallback)}
     * @param path
     * @param recordChangedCallback
     * @return
     * @throws DeepstreamRecordDestroyedException
     */
    public Record unsubscribe( String path, RecordChangedCallback recordChangedCallback ) throws DeepstreamRecordDestroyedException {
        throwExceptionIfDestroyed( "unsubscribe" );

        if( path == null ) {
            this.subscribers.off( ALL_EVENT, recordChangedCallback );
        } else {
            this.subscribers.off( path, recordChangedCallback );
        }

        return this;
    }

    /**
     * Discard the record. This should be called whenever you are done with the record retrieved by {@link RecordHandler#getRecord(String)}.
     * This does not guarantee that your subscriptions have been unsubscribed, so make sure to do that first!
     * If all usages of the same record have been discarded, the record will no longer be updated from the server and
     * any further usages will require the record to be retrieved again via {@link RecordHandler#getRecord(String)}
     *
     * Once the record is successfully discard, you can be notified via {@link RecordEventsListener#onRecordDiscarded(String)}
     * @return
     * @throws DeepstreamRecordDestroyedException
     */
    public Record discard() throws DeepstreamRecordDestroyedException {
        throwExceptionIfDestroyed( "delete" );
        this.usages--;
        if( this.usages <= 0 ) {
            // TODO: on ready async callback
            int subscriptionTimeout = Integer.parseInt( (String) this.options.get( "subscriptionTimeout" ) );
            this.ackTimeoutRegistry.add( Topic.RECORD, Actions.UNSUBSCRIBE, name, subscriptionTimeout );
            this.connection.send( MessageBuilder.getMsg( Topic.RECORD, Actions.UNSUBSCRIBE, this.name ) );
        }
        return this;
    }

    /**
     * Delete the record. This is called when you want to remove the record entirely from deepstream, deleting it from storage
     * and cache and telling all other users that it has been deleted. This in turn will force all clients to discard the record.
     *
     * Once the record is successfully deleted, you can be notified via {@link RecordEventsListener#onRecordDeleted(String)} (String)}
     *
     * @return
     * @throws DeepstreamRecordDestroyedException
     */
    public Record delete() throws DeepstreamRecordDestroyedException {
        throwExceptionIfDestroyed( "delete" );

        // TODO: on ready async callback
        int subscriptionTimeout = Integer.parseInt( (String) this.options.get( "recordDeleteTimeout" ) );
        this.ackTimeoutRegistry.add( Topic.RECORD, Actions.DELETE, name, Event.DELETE_TIMEOUT, subscriptionTimeout );
        this.connection.send( MessageBuilder.getMsg( Topic.RECORD, Actions.DELETE, this.name ) );

        return this;
    }

    /**
     * Add a recordReadyListener as a callback. This means it will be called once when the record is ready, either in sync
     * or async if the record is not already ready.
     * TODO: Discuss whether we want this to be blocking. If that is the case, we would need to look at other APIs too.
     * @param recordReadyListener
     * @return
     */
    public Record whenReady( RecordReadyListener recordReadyListener ) {
        if( this.isReady ) {
            recordReadyListener.onRecordReady( this.name, this );
        } else {
            this.onceRecordReadyListeners.add( recordReadyListener );
        }
        return this;
    }

    /**
     * Inovoked when a message is recieved from {@link RecordHandler#handle(Message)}
     * @param message
     */
    protected void onMessage(Message message) {
        if( message.action == Actions.ACK ) {
            processAckMessage( message );
        } else if( message.action == Actions.READ && this.version == -1 ) {
            onRead( message );
        } else if( message.action == Actions.READ || message.action == Actions.UPDATE || message.action == Actions.PATCH ) {
            applyUpdate( message );
        } else if( message.data[ 0 ] == Event.VERSION_EXISTS.toString() ) {
            recoverRecord( Integer.parseInt( message.data[ 2 ] ), gson.fromJson( message.data[ 3 ], JsonElement.class ), message );
        } else if( message.data[ 0 ] == Event.MESSAGE_DENIED.toString() ) {
           clearTimeouts();
        }
    }

    /**
     * This gives us a handle to before and after a record is updated remotely. This is currently used by {@link io.deepstream.List}
     * @param recordRemoteUpdateListener
     */
    void setRecordRemoteUpdateListener( RecordRemoteUpdateListener recordRemoteUpdateListener ) {
        this.recordRemoteUpdateListener = recordRemoteUpdateListener;
    }

    /**
     * Apply the message received on the server on the record
     * @param message
     */
    private void applyUpdate(Message message) {
        int version = Integer.parseInt( message.data[ 1 ] );

        JsonElement data;
        if( message.action == Actions.PATCH ) {
            data = gson.toJsonTree( MessageParser.convertTyped( message.data[ 3 ], client ) );
        } else {
            data = gson.fromJson( message.data[ 2 ], JsonElement.class );
        }


        if( this.version != -1 && this.version + 1 != version ) {
            if( message.action == Actions.PATCH ) {
                /**
                 * Request a snapshot so that a merge can be done with the read reply which contains
                 * the full state of the record
                 **/
                this.connection.send( MessageBuilder.getMsg( Topic.RECORD, Actions.SNAPSHOT, this.name ) );
            } else {
                recoverRecord( version, data, message );
            }
            return;
        }

        if( this.recordRemoteUpdateListener != null ) {
            this.recordRemoteUpdateListener.beforeRecordUpdate();
        }

        Map oldValues = beginChange();

        this.version = version;
        if( Actions.PATCH == message.action ) {
            path.set( message.data[ 2 ], data );
            System.out.println( "Changed to: " + path.get( message.data[2] ));
        } else {
            this.data = data;
            this.path.setCoreElement( data );
        }

        completeChange( oldValues );

        if( this.recordRemoteUpdateListener != null ) {
            this.recordRemoteUpdateListener.afterRecordUpdate();
        }
    }

    /**
     * Called when a merge conflict is detected by a VERSION_EXISTS error or if an update recieved
     * is directly after the clients. If no merge strategy is configure it will emit a VERSION_EXISTS
     * error and the record will remain in an inconsistent state.
     * @param remoteVersion The remote version number
     * @param remoteData The remote object data
     * @param message parsed and validated deepstream message
     */
    private void recoverRecord(int remoteVersion, JsonElement remoteData, Message message) {
        try {
            JsonElement mergedData = this.mergeStrategy.merge( this, remoteData, remoteVersion );
            this.version = remoteVersion;
            this.set( null, mergedData, true );
        } catch( RecordMergeStrategyException ex ) {
            this.client.onError( Topic.RECORD, Event.VERSION_EXISTS, "Received update for " + remoteVersion + " but version is " + this.version );
        }
    }

    /**
     * Start response timeouts
     */
    private void scheduleAcks() {
        int readAckTimeout = Integer.parseInt( (String) this.options.get( "recordReadAckTimeout" ) );
        this.ackTimeoutRegistry.add( Topic.RECORD, Actions.SUBSCRIBE, this.name, Event.ACK_TIMEOUT, readAckTimeout );

        int readResponseTimeout = Integer.parseInt( (String) this.options.get( "recordReadTimeout" ) );
        this.ackTimeoutRegistry.add( Topic.RECORD, Actions.READ, this.name, Event.RESPONSE_TIMEOUT, readResponseTimeout );
    }

    /**
     * Remove response timeouts
     */
    private void clearTimeouts() {
        this.ackTimeoutRegistry.clear( Topic.RECORD, Actions.SUBSCRIBE, this.name );
        this.ackTimeoutRegistry.clear( Topic.RECORD, Actions.READ, this.name );
    }

    /**
     * First of two steps that are called for incoming and outgoing updates.
     * Saves the current value of all paths the app is subscribed to.
     * @return
     */
    private Map beginChange() {
        Set<String> paths = this.subscribers.getEvents();

        if( paths.isEmpty() ) {
            return null;
        }

        Map<String,JsonElement> oldValues = new HashMap();

        if( paths.contains( ALL_EVENT ) ) {
            oldValues.put( ALL_EVENT, this.get() );
        }

        for( String path : paths ) {
            if( path != ALL_EVENT ) {
                oldValues.put( path, this.get( path ) );
            }
        }

        return oldValues;
    }

    /**
     * Second of two steps that are called for incoming and outgoing updates.
     * Compares the new values for every path with the previously stored ones and
     * updates the subscribers if the value has changed
     *
     * @param oldValues
     * @return
     */
    private void completeChange(Map<String,JsonElement> oldValues) {
        List<Object> listeners;

        JsonElement oldValue, newValue;

        if( oldValues == null || oldValues.isEmpty() ) {
            return;
        }

        oldValue = oldValues.remove( ALL_EVENT );
        if( oldValue != null && !oldValue.equals( this.data ) ) {
            listeners = this.subscribers.listeners( ALL_EVENT );
            for( Object listener : listeners ) {
                ((RecordChangedCallback) listener).onRecordChanged( this.name, this.get() );
            }
        }

        for( String key : oldValues.keySet() ) {
            oldValue = oldValues.get( key );
            newValue = this.get( key );
            if( oldValue == null || !oldValue.equals( newValue ) ) {
                listeners = this.subscribers.listeners( key );
                for( Object listener : listeners ) {
                    ((RecordChangedCallback) listener).onRecordChanged( this.name, key, newValue );
                }
            }
        }
    }

    /**
     * Throw an exception if the record has been destroyed
     * @param method
     * @throws DeepstreamRecordDestroyedException
     */
    private void throwExceptionIfDestroyed(String method) throws DeepstreamRecordDestroyedException {
        if( this.isDestroyed ) {
            throw new DeepstreamRecordDestroyedException( method );
        }
    }

    /**
     *
     * @param message
     */
    private void processAckMessage(Message message) {
        Actions action = Actions.getAction( message.data[ 0 ] );
        this.ackTimeoutRegistry.clear( message );

        if( action.equals( Actions.DELETE ) ) {
            for(RecordEventsListener recordEventsListener: this.recordEventsListeners) {
                recordEventsListener.onRecordDeleted( this.name );
            }
            this.destroy();
        }
        else if( action.equals( Actions.UNSUBSCRIBE ) ) {
            for(RecordEventsListener recordEventsListener: this.recordEventsListeners) {
                recordEventsListener.onRecordDiscarded( this.name );
            }
            this.destroy();
        }
    }

    /**
     * Callback for incoming read messages
     * @param message
     */
    private void onRead( Message message ) {
        ackTimeoutRegistry.clear( message );

        Map oldValues = beginChange();
        this.version = Integer.parseInt( message.data[ 1 ] );
        this.data = gson.fromJson( message.data[ 2 ], JsonElement.class );
        this.path.setCoreElement(this.data);
        completeChange( oldValues );
        setReady();
    }

    /**
     * Invokes method calls that where queued while the record wasn't ready
     * and emits the ready event
     */
    private void setReady() {
        this.isReady = true;

        for(RecordReadyListener recordReadyListener: this.onceRecordReadyListeners) {
            recordReadyListener.onRecordReady( this.name, this );
        }
        this.onceRecordReadyListeners.clear();

        for(RecordReadyListener recordReadyListener: this.recordReadyListeners) {
            recordReadyListener.onRecordReady( this.name, this );
        }
    }

    /**
     * Sends the read message, either initially at record
     * creation or after a lost connection has been re-established
     */
    private void sendRead() {
        if( this.client.getConnectionState() == ConnectionState.OPEN ) {
            this.connection.send( MessageBuilder.getMsg( Topic.RECORD, Actions.CREATEORREAD, this.name ) );
        }
    }

    /**
     * Send the update to the server, either as an update or patch
     * @param key
     * @param value
     */
    private void sendUpdate( String key, Object value ) {
        if( key == null || key == "" ) {
            this.connection.sendMsg( Topic.RECORD, Actions.UPDATE, new String[] {
                    this.name,
                    String.valueOf( this.version ),
                    gson.toJson( value )
            });
        }
        else {
            this.connection.sendMsg( Topic.RECORD, Actions.PATCH, new String[] {
                    this.name,
                    String.valueOf( this.version ),
                    key,
                    MessageBuilder.typed( value )
            });
        }
    }

    /**
     * Destroys the record and nulls all
     * its dependencies
     */
    private void destroy() {
        this.clearTimeouts();
        this.utilResubscribeNotifier.destroy();
        this.isReady = false;
        this.isDestroyed = true;
    }

    /**
     * Generate a deepcopy of the object to prevent user to modify record data directly
     * @param element
     * @return
     */
    private JsonElement deepCopy(JsonElement element) {
        try {
            return gson.fromJson(gson.toJson(element, JsonElement.class), JsonElement.class);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Generate a deepcopy of the object and cast it to a class of any type, used by {@link io.deepstream.List}
     * @param element
     * @return
     */
    private <T> T deepCopy(JsonElement element, Class<T> type) {
        return gson.fromJson(gson.toJson(element, JsonElement.class), type);
    }

    static interface RecordRemoteUpdateListener {
        void beforeRecordUpdate();
        void afterRecordUpdate();
    }
}
