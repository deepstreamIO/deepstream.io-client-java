package io.deepstream;


import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import com.google.j2objc.annotations.ObjectiveCName;

import java.util.*;
import java.util.List;


/**
 * This class represents a single record - an observable
 * dataset returned by {@link RecordHandler#getRecord(String)}
 */
public class Record {
    private static final String ALL_EVENT = "ALL_EVENT";
    private static final String DESTROY_PENDING = "DESTROY_PENDING";
    private final UtilResubscribeNotifier utilResubscribeNotifier;
    private final UtilAckTimeoutRegistry ackTimeoutRegistry;
    private final IConnection connection;
    private final DeepstreamClientAbstract client;
    private final Gson gson;
    private final UtilJSONPath path;
    private final UtilEmitter subscribers;
    private final ArrayList<RecordEventsListener> recordEventsListeners;
    private final ArrayList<Record.RecordDestroyPendingListener> recordDestroyPendingListeners;
    private final ArrayList<RecordReadyListener> onceRecordReadyListeners;
    private final String name;
    private final DeepstreamConfig deepstreamConfig;
    private boolean isReady;
    private boolean isDestroyed;
    private int version;
    private int usages;
    private RecordMergeStrategy mergeStrategy;
    private RecordRemoteUpdateHandler recordRemoteUpdateHandler;
    private JsonElement data;
    private boolean hasProvider;

    /**
     * Constructor is not public since it is created via {@link RecordHandler#getRecord(String)}
     * @param name The unique name of the record
     * @param recordOptions A map of deepstreamConfig, e.g. { persist: true }
     * @param connection The instance of the server connection
     * @param deepstreamConfig Deepstream deepstreamConfig
     * @param client deepstream.io client
     */
    @ObjectiveCName("init:recordOptions:connection:deepstreamConfig:client:")
    Record(String name, Map recordOptions, IConnection connection, DeepstreamConfig deepstreamConfig, DeepstreamClientAbstract client) {
        this.ackTimeoutRegistry = client.getAckTimeoutRegistry();
        this.name = name;
        this.deepstreamConfig = deepstreamConfig;
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
        this.hasProvider = false;

        this.recordEventsListeners = new ArrayList<>();
        this.onceRecordReadyListeners = new ArrayList<>();
        this.recordDestroyPendingListeners = new ArrayList<>();

        this.utilResubscribeNotifier = new UtilResubscribeNotifier(client, new UtilResubscribeNotifier.UtilResubscribeListener() {
            @Override
            public void resubscribe() {
                sendRead();
            }
        });
    }

    /**
     * Send the subscriber request to the server
     */
    void start() {
        this.scheduleAcks();
        this.sendRead();
    }

    /**
     * Return whether the record data has been loaded from the server
     * @return true if record has been loaded
     */
    public boolean isReady() {
        return this.isReady;
    }

    /**
     * Return whether the record has an active provider
     * @return true if record has been loaded
     */
    public boolean hasProvider() {
        return this.hasProvider;
    }

    /**
     * Return whether the record data has been destroyed. If true and you need to use the method create it again via
     * {@link RecordHandler#getRecord(String)}
     * @return true if record has been destroyed
     */
    public boolean isDestroyed() {
        return this.isDestroyed;
    }

    /**
     * Return the record version. This is solely used within a {@link RecordMergeStrategy}.
     * @return -1 if not loaded, otherwise the local version number
     */
    public int version() {
        return this.version;
    }

    /**
     * Return the record name
     * @return The record name
     */
    public String name() {
        return this.name;
    }

    /**
     * Adds a Listener that will notify you if a Discard, Delete or Error event occurs
     * @param recordEventsListener The listener to add
     * @return The record
     */
    @ObjectiveCName("addRecordEventsListener:")
    public Record addRecordEventsListener(RecordEventsListener recordEventsListener) {
        this.recordEventsListeners.add( recordEventsListener );
        return this;
    }

    /**
     * Remove listener added via {@link io.deepstream.Record#addRecordEventsListener(RecordEventsListener)}
     * @param recordEventsListener The listener to remove
     * @return The record
     */
    @ObjectiveCName("removeRecordEventsListener:")
    public Record removeRecordEventsListener(RecordEventsListener recordEventsListener) {
        this.recordEventsListeners.remove( recordEventsListener );
        return this;
    }

    /**
     * Set a merge strategy that comes with deepstream. These are currently LOCAL_WINS and REMOTE_WINS
     * @param mergeStrategy The name of the built in merge strategy to use
     * @return The record
     */
    @ObjectiveCName("setMergeStrategy:")
    public Record setMergeStrategy(MergeStrategy mergeStrategy) {
        this.mergeStrategy = RecordMergeStrategies.INSTANCE.getMergeStrategy( mergeStrategy );
        return this;
    }

    /**
     * Set a custom merge strategy for this record
     * @param mergeStrategy The custom merge strategy to use
     * @return The record
     */
    @ObjectiveCName("setMergeStrategyWithRecordMergeStrategy:")
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
     * @return The object with the type passed in and containing the records data
     */
    <T> T get( Class<T> type ) {
        return deepCopy( this.data, type );
    }

    /**
     * Gets the value at the path indicated.<br/>
     * <br/>
     * For example, if the record data is:<br/>
     * { "name": "Yasser", pets: [ { type: "Dog", "name": "Whiskey", age: 3} ]}<br/>
     * <br/>
     * We can do:<br/>
     * get( "name" ) -> {@link JsonElement#getAsString()}<br/>
     * get( "pets[0]" ) -> {@link JsonElement#getAsJsonObject()}<br/>
     * get( "pets[0].age") -> {@link JsonElement#getAsInt()}<br/>
     *
     * @return The record data as a JsonElement
     */
    @ObjectiveCName("get:")
    public JsonElement get( String path ) {
        return deepCopy( this.path.get( path ) );
    }

    /**
     * Gets the entire record data and should always return a {@link JsonObject}, except when using
     * a {@link io.deepstream.List}, but then you should always be using it via {@link io.deepstream.List#getEntries()} ;)
     *
     * @see Record#get(String)
     *
     * @return The record data as a json element
     */
    public JsonElement get() {
        return deepCopy( this.data );
    }

    /**
     * Set the value for the entire record<br/>
     * Make sure that the Object passed in can be serialised to a JsonElement, otherwise it will
     * throw a {@link IllegalStateException}. Best way to guarantee this is by setting Json friendly objects,
     * such as {@link Map}. Since this is a root the object should also not be a primitive.
     *
     * @see Record#set(String, Object)
     */ 
    @ObjectiveCName("set:")
    public Record set( Object value ) throws DeepstreamRecordDestroyedException {
        return this.set( null, value, false );
    }

    /**
     * Set the value for a specific path in your Record data.<br/>
     * Make sure that the Object passed in can be serialised to a JsonElement, otherwise it will
     * throw a {@link IllegalStateException}.<br/>
     * The best way to guarantee this is by setting Json friendly objects,
     * such as {@link Map}.<br/>
     * If you path is not null, you can pass in primitives as long as the path
     * is not null, which is the equivalent of calling {@link Record#set(Object)}.
     *
     * @param path The path with the JsonElement at which to set the value
     * @param value The value to set
     * @return The record
     * @throws DeepstreamRecordDestroyedException Thrown if the record has been destroyed and can't perform more actions
     */
    @ObjectiveCName("set:value:")
    public Record set(String path, Object value ) throws DeepstreamRecordDestroyedException {
        return this.set( path, value, false );
    }

    /**
     * Notifies the user whenever anything under the path provided has changed.
     * @see Record#subscribe(String, RecordPathChangedCallback, boolean)
     */
    @ObjectiveCName("subscribe:recordPathChangedCallback:")
    public Record subscribe(String path, RecordPathChangedCallback recordPathChangedCallback ) throws DeepstreamRecordDestroyedException {
        return subscribe( path, recordPathChangedCallback, false );
    }

    /**
     * Subscribe to record changes.<br/>
     *
     * If a path is provided, updates will be based on everything in or under it.<br/>
     *
     * If trigger now is true, the listener will be immediately fired with the current value.
     *
     * @param path The path to listen to
     * @param recordPathChangedCallback The listener to add
     * @param triggerNow Whether to immediately trigger the listener with the current value
     * @return The record
     * @throws DeepstreamRecordDestroyedException Thrown if the record has been destroyed and can't perform more actions
     */
    @ObjectiveCName("subscribe:recordPathChangedCallback:triggerNow:")
    public Record subscribe( String path, RecordPathChangedCallback recordPathChangedCallback, boolean triggerNow ) throws DeepstreamRecordDestroyedException {
        throwExceptionIfDestroyed( "subscribe" );

        this.subscribers.on( path, recordPathChangedCallback );

        if( triggerNow ) {
            recordPathChangedCallback.onRecordPathChanged( this.name, path, this.get( path ) );
        }

        return this;
    }

    /**
     * Notifies the user whenever anything inside the Record has changed.
     * @see Record#subscribe(RecordChangedCallback, boolean)
     */
    @ObjectiveCName("subscribe:")
    public Record subscribe( RecordChangedCallback recordChangedCallback ) throws DeepstreamRecordDestroyedException {
        return subscribe( recordChangedCallback, false );
    }

    /**
     *  Notifies the user whenever anything inside the Record has changed, and triggers the listener immediately.
     *  @param recordChangedCallback The callback for whenever anything within the record changes
     *  @param triggerNow Whether to call the callback immediately with the current value
     */
    @ObjectiveCName("subscribe:triggerNow:")
    public Record subscribe( RecordChangedCallback recordChangedCallback, boolean triggerNow ) throws DeepstreamRecordDestroyedException {
        throwExceptionIfDestroyed( "subscribe" );

        this.subscribers.on( ALL_EVENT, recordChangedCallback );

        if( triggerNow ) {
            recordChangedCallback.onRecordChanged( this.name, this.get() );
        }

        return this;
    }

    /**
     * Remove the listener added via {@link Record#subscribe(RecordChangedCallback, boolean)}
     *
     * @param recordChangedCallback The listener to remove
     * @throws DeepstreamRecordDestroyedException Thrown if the record has been destroyed and can't perform more actions
     * @return The record
     */
    @ObjectiveCName("unsubscribe:")
    public Record unsubscribe( RecordChangedCallback recordChangedCallback ) throws DeepstreamRecordDestroyedException {
        throwExceptionIfDestroyed( "unsubscribe" );
        this.subscribers.off( ALL_EVENT, recordChangedCallback );
        return this;
    }

    /**
     * Remove the listener added via {@link Record#subscribe(String,RecordPathChangedCallback, boolean)}
     * @param path The path to unsubscribe from
     * @param recordPathChangedCallback The listener to remove
     * @throws DeepstreamRecordDestroyedException Thrown if the record has been destroyed and can't perform more actions
     * @return The record
     */
    @ObjectiveCName("unsubscribe:recordPathChangedCallback:")
    public Record unsubscribe( String path, RecordPathChangedCallback recordPathChangedCallback ) throws DeepstreamRecordDestroyedException {
        throwExceptionIfDestroyed( "unsubscribe" );
        this.subscribers.off( path, recordPathChangedCallback );
        return this;
    }

    /**
     * Discard the record. This should be called whenever you are done with the record retrieved by {@link RecordHandler#getRecord(String)}.
     * This does not guarantee that your subscriptions have been unsubscribed, so make sure to do that first!<br/>
     *
     * If all usages of the same record have been discarded, the record will no longer be updated from the server and
     * any further usages will require the record to be retrieved again via {@link RecordHandler#getRecord(String)}<br/>
     *
     * Once the record is successfully discard, you can be notified via {@link RecordEventsListener#onRecordDiscarded(String)}
     * @return The record
     * @throws DeepstreamRecordDestroyedException Thrown if the record has been destroyed and can't perform more actions
     */
    public Record discard() throws DeepstreamRecordDestroyedException {
        throwExceptionIfDestroyed("discard");
        this.usages--;
        if( this.usages <= 0 ) {
            this.whenReady(new RecordReadyListener() {
                @Override
                public void onRecordReady(String recordName, Record record) {
                    ackTimeoutRegistry.add(Topic.RECORD, Actions.UNSUBSCRIBE, name, deepstreamConfig.getSubscriptionTimeout());
                    connection.send( MessageBuilder.getMsg( Topic.RECORD, Actions.UNSUBSCRIBE, name ) );

                    for(RecordDestroyPendingListener recordDestroyPendingHandler: recordDestroyPendingListeners) {
                        recordDestroyPendingHandler.onDestroyPending( name );
                    }
                }
            });
        }
        return this;
    }

    /**
     * Delete the record. This is called when you want to remove the record entirely from deepstream, deleting it from storage
     * and cache and telling all other users that it has been deleted. This in turn will force all clients to discard the record.<br/>
     *
     * Once the record is successfully deleted, you can be notified via {@link RecordEventsListener#onRecordDeleted(String)} (String)}</br>
     *
     * @return The record
     * @throws DeepstreamRecordDestroyedException Thrown if the record has been destroyed and can't perform more actions
     */
    public Record delete() throws DeepstreamRecordDestroyedException {
        throwExceptionIfDestroyed( "delete" );

        this.whenReady(new RecordReadyListener() {
            @Override
            public void onRecordReady(String recordName, Record record) {
                ackTimeoutRegistry.add(Topic.RECORD, Actions.DELETE, name, Event.DELETE_TIMEOUT, deepstreamConfig.getSubscriptionTimeout());
                connection.send( MessageBuilder.getMsg( Topic.RECORD, Actions.DELETE, name ) );

                for(RecordDestroyPendingListener recordDestroyPendingHandler: recordDestroyPendingListeners) {
                    recordDestroyPendingHandler.onDestroyPending( name );
                }
            }
        });


        return this;
    }

    /**
     * Add a recordReadyListener as a callback. This means it will be called once when the record is ready, either in sync
     * or async if the record is not already ready.
     * @param recordReadyListener The recordReadyListener that will be triggered only **once**
     * @return The record
     */
    @ObjectiveCName("whenReady:")
    Record whenReady(RecordReadyListener recordReadyListener) {
        if( this.isReady ) {
            recordReadyListener.onRecordReady( this.name, this );
        } else {
            this.onceRecordReadyListeners.add( recordReadyListener );
        }
        return this;
    }

    /**
     * Invoked when a message is received from {@link RecordHandler#handle(Message)}
     * @param message The message received from the server
     */
    @ObjectiveCName("onMessage:")
    void onMessage(Message message) {
        if( message.action == Actions.ACK ) {
            processAckMessage( message );
        } else if (message.action == Actions.READ && this.version() == -1) {
            onRead( message );
        } else if( message.action == Actions.READ || message.action == Actions.UPDATE || message.action == Actions.PATCH ) {
            applyUpdate( message );
        } else if (message.action == Actions.SUBSCRIPTION_HAS_PROVIDER) {
            updateHasProvider(message);
        } else if( message.data[ 0 ].equals( Event.VERSION_EXISTS.toString() ) ) {
            recoverRecord( Integer.parseInt( message.data[ 2 ] ), gson.fromJson( message.data[ 3 ], JsonElement.class ));
        } else if( message.data[ 0 ].equals( Event.MESSAGE_DENIED.toString() ) ) {
           clearTimeouts();
        }
    }

    /**
     * This gives us a handle to before and after a record is updated remotely. This is currently used by {@link io.deepstream.List}
     * @param recordRemoteUpdateHandler The listener to notify before and after an update is applied
     */
    @ObjectiveCName("setRecordRemoteUpdateHandler:")
    void setRecordRemoteUpdateHandler(RecordRemoteUpdateHandler recordRemoteUpdateHandler) {
        this.recordRemoteUpdateHandler = recordRemoteUpdateHandler;
    }

    @ObjectiveCName("updateHasProvider:")
    private void updateHasProvider(Message message) {
        this.hasProvider = (boolean) MessageParser.convertTyped(message.data[1], this.client);
        for (RecordEventsListener recordEventsListener : this.recordEventsListeners) {
            recordEventsListener.onRecordHasProviderChanged(this.name, this.hasProvider);
        }
    }

    /**
     * Apply the message received on the server on the record
     */
    @ObjectiveCName("applyUpdate:")
    private void applyUpdate(Message message) {
        int newVersion = Integer.parseInt(message.data[1]);

        JsonElement data;
        if( message.action == Actions.PATCH ) {
            data = gson.toJsonTree( MessageParser.convertTyped( message.data[ 3 ], client ) );
        } else {
            data = gson.fromJson( message.data[ 2 ], JsonElement.class );
        }


        if (this.version != -1 && this.version + 1 != newVersion) {
            if( message.action == Actions.PATCH ) {
                /*
                  Request a snapshot so that a merge can be done with the read reply which contains
                  the full state of the record
                 */
                this.connection.send( MessageBuilder.getMsg( Topic.RECORD, Actions.SNAPSHOT, this.name ) );
            } else {
                recoverRecord(newVersion, data);
            }
            return;
        }

        if( this.recordRemoteUpdateHandler != null ) {
            this.recordRemoteUpdateHandler.beforeRecordUpdate();
        }

        Map<String, JsonElement> oldValues = beginChange();

        this.version = newVersion;
        if( Actions.PATCH == message.action ) {
            path.set( message.data[ 2 ], data );
        } else {
            this.data = data;
            this.path.setCoreElement( data );
        }

        completeChange( oldValues );

        if( this.recordRemoteUpdateHandler != null ) {
            this.recordRemoteUpdateHandler.afterRecordUpdate();
        }
    }

    /**
     * Called when a merge conflict is detected by a VERSION_EXISTS error or if an update received
     * is directly after the clients. If no merge strategy is configure it will emit a VERSION_EXISTS
     * error and the record will remain in an inconsistent state.
     * @param remoteVersion The remote version number
     * @param remoteData The remote object data
     */
    @ObjectiveCName("recoverRecord:remoteData:")
    private void recoverRecord(int remoteVersion, JsonElement remoteData) {
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
        this.ackTimeoutRegistry.add(Topic.RECORD, Actions.SUBSCRIBE, this.name, Event.ACK_TIMEOUT, deepstreamConfig.getRecordReadAckTimeout());
        this.ackTimeoutRegistry.add(Topic.RECORD, Actions.READ, this.name, Event.RESPONSE_TIMEOUT, deepstreamConfig.getRecordReadTimeout());
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
     * @return The record
     */
    private Map<String,JsonElement> beginChange() {
        Set<String> paths = this.subscribers.getEvents();

        if( paths.isEmpty() ) {
            return null;
        }

        Map<String,JsonElement> oldValues = new HashMap<>();

        if( paths.contains( ALL_EVENT ) ) {
            oldValues.put( ALL_EVENT, this.get() );
        }

        for( String path : paths ) {
            if( !path.equals( ALL_EVENT ) ) {
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
     * @param oldValues The previous paths and values
     */
    // @ObjectiveCName("completeChange:")
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
                    if( listener instanceof RecordPathChangedCallback ) {
                        ((RecordPathChangedCallback) listener).onRecordPathChanged( this.name, key, newValue );
                    }
                }
            }
        }
    }

    /**
     * Throw an exception if the record has been destroyed
     * @param method The method to call
     * @throws DeepstreamRecordDestroyedException Thrown if the record has been destroyed and can't perform more actions
     */
    // @ObjectiveCName("throwExceptionIfDestroyed:")
    private void throwExceptionIfDestroyed(String method) throws DeepstreamRecordDestroyedException {
        if( this.isDestroyed ) {
            throw new DeepstreamRecordDestroyedException( method );
        }
    }

    /**
     * @param message The ack {@link Message}
     */
    @ObjectiveCName("processAckMessage:")
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
     * @param message The read {@link Message}
     */
    @ObjectiveCName("onRead:")
    private void onRead( Message message ) {
        ackTimeoutRegistry.clear( message );

        Map<String,JsonElement> oldValues = beginChange();
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
     * @param key The key to update if a patch
     * @param value The value to update the record with
     */
    @ObjectiveCName("sendUpdate:value:")
    private void sendUpdate( String key, Object value ) {
        if( key == null || key.equals("") ) {
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
     * Generate a deep copy of the object to prevent user to modify record data directly
     */
    @ObjectiveCName("deepCopy:")
    private JsonElement deepCopy(JsonElement element) {
        try {
            return gson.fromJson(gson.toJson(element, JsonElement.class), JsonElement.class);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Generate a deep copy of the object and cast it to a class of any type, used by {@link io.deepstream.List}
     */
    private <T> T deepCopy(JsonElement element, Class<T> type) {
        return gson.fromJson(gson.toJson(element, JsonElement.class), type);
    }

    /**
     * This forces an update, which is useful when trying to reconcile a merge conflict when the merge is the same
     * but the version number isn't.
     */
    @ObjectiveCName("set:value:force:")
    private Record set(String path, Object value, boolean force ) throws DeepstreamRecordDestroyedException {
        throwExceptionIfDestroyed( "set" );

        JsonElement element = gson.toJsonTree( value );
        JsonElement object = this.path.get( path );

        if( !force ) {
            if( object != null && object.equals( value ) ) {
                return this;
            } else if( path == null && this.data.equals( value ) ) {
                return this;
            }
        }

        Map<String,JsonElement> oldValues = beginChange();
        this.version++;
        this.path.set( path, element );
        this.data = this.path.getCoreElement();
        sendUpdate( path, value );
        completeChange( oldValues );

        return this;
    }

    /**
     * Add a destroy pending listener, used by the RecordHandler and potentially other internal stores
     */
    @ObjectiveCName("addRecordDestroyPendingListener:")
    void addRecordDestroyPendingListener(RecordDestroyPendingListener recordDestroyPendingListener) {
        this.recordDestroyPendingListeners.add( recordDestroyPendingListener );
    }

    void incrementUsage() {
        this.usages++;
    }

    interface RecordRemoteUpdateHandler {
        /**
         * Called before a remote update is applied to the current data
         */
        void beforeRecordUpdate();
        /**
         * Called after a remote update is applied to the current data
         */
        void afterRecordUpdate();
    }

    interface RecordDestroyPendingListener {
        /**
         * Called whenever the client is about to send the server a {@link Record#discard()} or {@link Record#delete()} event.<br/>
         * This should not be required to be implemented
         * @param recordName The name of the record being destroyed
         */
        void onDestroyPending(String recordName);
    }

    /**
     * A listener that notifies the user whenever the record state is ready.
     */
    interface RecordReadyListener {
        /**
         * Called when the record is loaded from the server
         *
         * @param recordName The name of the record which is now ready
         * @param record     The record which is now ready / loaded from server
         */
        void onRecordReady(String recordName, Record record);
    }
}
