package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import com.google.gson.JsonElement;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CountDownLatch;

/**
 * The getters for data-sync, such as {@link RecordHandler#getRecord(String)},
 * {@link RecordHandler#getList(String)}, provider functionality such as {@link RecordHandler#listen(String, ListenListener)}
 * and single requests like {@link RecordHandler#snapshot(String)}
 */
public class RecordHandler {

    private final DeepstreamConfig deepstreamConfig;
    private final IConnection connection;
    private final DeepstreamClientAbstract client;
    private final Map<String, Record> records;
    private final Map<String, List> lists;
    private final UtilSingleNotifier hasRegistry;
    private final UtilSingleNotifier snapshotRegistry;
    private final Map<String, UtilListener> listeners;
    private final RecordHandlerListeners recordHandlerListeners;

    /**
     * A collection of factories for records. This class
     * is exposed as client.record
     *
     * @param deepstreamConfig The deepstreamConfig the client was created with
     * @param connection The connection
     * @param client The deepstream client
     */
    @ObjectiveCName("init:connection:client:")
    RecordHandler(DeepstreamConfig deepstreamConfig, IConnection connection, DeepstreamClientAbstract client) {
        this.deepstreamConfig = deepstreamConfig;
        this.connection = connection;
        this.client = client;
        recordHandlerListeners = new RecordHandlerListeners();

        records = new HashMap<>();
        lists = new HashMap<>();
        listeners = new HashMap<>();

        hasRegistry = new UtilSingleNotifier(client, connection, Topic.RECORD, Actions.HAS, deepstreamConfig.getRecordReadTimeout());
        snapshotRegistry = new UtilSingleNotifier(client, connection, Topic.RECORD, Actions.SNAPSHOT, deepstreamConfig.getRecordReadTimeout());
    }

    /**
     * Returns an existing record or creates a new one. If creating a new one the record
     * will not be in a ready state till it is loaded from the server.
     * @param name The name of the record to get
     * @return Record The record
     */
    @ObjectiveCName("getRecord:")
    public Record getRecord( String name ) {
        Record record = records.get( name );
        if( record == null ) {
            synchronized (this) {
                record = new Record(name, new HashMap(), connection, deepstreamConfig, client);
                records.put(name, record);
                record.addRecordEventsListener(recordHandlerListeners);
                record.addRecordDestroyPendingListener(recordHandlerListeners);
                record.start();
            }
        }
        record.incrementUsage();

        if (!record.isReady()) {
            final CountDownLatch readyLatch = new CountDownLatch(1);
            record.whenReady(new Record.RecordReadyListener() {
                @Override
                public void onRecordReady(String recordName, Record record) {
                    readyLatch.countDown();
                }
            });
            try {
                readyLatch.await();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        return record;
    }

    /**
     * Returns an existing List or creates a new one. A list is a specialised
     * type of record that holds an array of recordNames.
     *
     * @param name The name of the list to retrieve
     * @return List The List
     */
    @ObjectiveCName("getList:")
    public List getList( String name ) {
        List list = lists.get( name );
        if( list == null ) {
            list = new List(this, name);
            lists.put(name, list);
        }
        return list;
    }

    /**
     * Returns an anonymous record. A anonymous record is effectively
     * a wrapper that mimicks the API of a record, but allows for the
     * underlying record to be swapped without losing subscriptions etc.<br/>
     *
     * This is particularly useful when selecting from a number of similarly
     * structured records. E.g. a list of users that can be choosen from a list<br/>
     *
     * The only API differences to a normal record is an additional {@link AnonymousRecord#setName(String)} method
     *
     * @return AnonymousRecord
     */
    public AnonymousRecord getAnonymousRecord() {
        return new AnonymousRecord( this );
    }

    /**
     * Allows to listen for record subscriptions made by this or other clients. This
     * is useful to create "active" data providers, e.g. providers that only provide
     * data for a particular record if a user is actually interested in it.<br/>
     *
     * You can only listen to a pattern once, and if multiple listeners match the same pattern only
     * a single one will be notified!
     *
     * @param pattern The pattern to match all records your interested in
     * @param listenCallback The listen callback when a match has been found or removed.
     */
    @ObjectiveCName("listen:listenCallback:")
    public void listen( String pattern, ListenListener listenCallback ) {
        if( listeners.containsKey( pattern ) ) {
            client.onError( Topic.RECORD, Event.LISTENER_EXISTS, pattern );
        } else {
            synchronized (this) {
                UtilListener utilListener = new UtilListener(Topic.RECORD, pattern, listenCallback, deepstreamConfig, client, connection);
                listeners.put(pattern, utilListener);
                utilListener.start();
            }
        }
    }

    /**
     * Removes a listener that was previously registered with listenForSubscriptions
     *
     * @param pattern The pattern to stop listening to
     */
    @ObjectiveCName("unlisten:")
    public void unlisten( String pattern ) {
        UtilListener listener = listeners.get( pattern );
        if( listener != null ) {
            listener.destroy();
            listeners.remove( pattern );
        } else {
            client.onError( Topic.RECORD, Event.NOT_LISTENING, pattern );
        }
    }

    /**
     * Retrieve the current record data without subscribing to changes<br/>
     *
     * If the record does not exist an error will be thrown
     *
     * @param name The name of the record which state to retrieve
     */
    @ObjectiveCName("snapshot:")
    public JsonElement snapshot(String name) throws DeepstreamError {
        final JsonElement[] data = new JsonElement[1];
        final DeepstreamError[] deepstreamException = new DeepstreamError[1];

        final Record record = records.get(name);

        if( record != null && record.isReady() ) {
            data[0] = record.get();
        } else {
            final CountDownLatch snapshotLatch = new CountDownLatch(1);

            snapshotRegistry.request(name, new UtilSingleNotifier.UtilSingleNotifierCallback() {
                @Override
                @ObjectiveCName("onSingleNotifierError:error:")
                public void onSingleNotifierError(String name, DeepstreamError error) {
                    deepstreamException[0] = error;
                    snapshotLatch.countDown();
                }

                @Override
                @ObjectiveCName("onSingleNotifierResponse:recordData:")
                public void onSingleNotifierResponse(String name, Object recordData) {
                    data[0] = (JsonElement) recordData;
                    snapshotLatch.countDown();
                }
            });

            try {
                snapshotLatch.await();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        if (deepstreamException[0] != null) {
            throw deepstreamException[0];
        }

        return data[0];
    }

    /**
     * Allows the user to query to see whether or not the record exists<br/>
     *
     * If the record is created locally the listener will be called sync, else
     * once the record is ready.<br/>
     *
     * @param name The name of the record to check
     */
    @ObjectiveCName("has:")
    public boolean has(String name) throws DeepstreamError {
        final DeepstreamError[] deepstreamException = new DeepstreamError[1];
        final boolean[] hasRecord = new boolean[1];

        Record record = records.get( name );
        if( record != null && record.isReady() ) {
            hasRecord[0] = true;
        } else {
            final CountDownLatch hasLatch = new CountDownLatch(1);

            hasRegistry.request(name, new UtilSingleNotifier.UtilSingleNotifierCallback() {
                @Override
                @ObjectiveCName("onSingleNotifierError:error:")
                public void onSingleNotifierError(String name, DeepstreamError error) {
                    deepstreamException[0] = error;
                    hasLatch.countDown();
                }

                @Override
                @ObjectiveCName("onSingleNotifierResponse:data:")
                public void onSingleNotifierResponse(String name, Object data) {
                    hasRecord[0] = (boolean) data;
                    hasLatch.countDown();
                }
            });

            try {
                hasLatch.await();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        if (deepstreamException[0] != null) {
            throw deepstreamException[0];
        }

        return hasRecord[0];
    }


    /**
     * Will be called by the client for incoming messages on the RECORD topic
     */
    @ObjectiveCName("handle:")
    protected void handle( Message message ) {
        Record record;
        boolean processed = false;
        String recordName;

        if( isUnhandledError( message ) ) {
            client.onError( Topic.RECORD, Event.getEvent( message.data[ 0 ] ), message.data[ 1 ] );
            return;
        }

        if( message.action == Actions.ACK || message.action == Actions.ERROR) {
            recordName = message.data[ 1 ];

            if( isDiscardAck( message ) ) {
                //TODO: destroyEventEmitter.emit( "destroy_ack_" + recordName, message );

                record = records.get( recordName );
                if( Actions.getAction( message.data[ 0 ] ) == Actions.DELETE && record != null ) {
                    record.onMessage( message );
                }

                return;
            }

            if( message.data[ 0 ].equals( Actions.SNAPSHOT.toString() ) ) {
                snapshotRegistry.recieve(recordName, new DeepstreamError(message.data[2]), null);
                return;
            }

            if( message.data[ 0 ].equals(Actions.HAS.toString() ))  {
                hasRegistry.recieve(recordName, new DeepstreamError(message.data[2]), null);
                return;
            }
        } else {
            recordName = message.data[ 0 ];
        }

        record = records.get( recordName );
        if( record != null ) {
            processed = true;
            record.onMessage( message );
        }

        if( message.action == Actions.READ && snapshotRegistry.hasRequest( recordName )) {
            processed = true;
            snapshotRegistry.recieve( recordName, null, MessageParser.parseObject( message.data[ 2 ] ) );
        }

        if( message.action == Actions.HAS && hasRegistry.hasRequest( recordName )) {
            processed = true;
            hasRegistry.recieve( recordName, null, MessageParser.convertTyped( message.data[ 1 ], client ) );
        }

        UtilListener listener = listeners.get( recordName );
        if( listener != null ) {
            processed = true;
            listener.onMessage( message );
        }

        if( !processed ) {
            client.onError(Topic.RECORD, Event.UNSOLICITED_MESSAGE, String.format("%s %s", message.action, recordName));
        }
    }

    /**
     * The following methods checks to prevent errors that occur when a record is discarded or deleted and
     * recreated before the discard / delete ack message is received.
     *
     * A (presumably unsolvable) problem remains when a client deletes a record in the exact moment
     * between another clients creation and read message for the same record
     */
    @ObjectiveCName("isDiscardAck:")
    private boolean isDiscardAck( Message message ) {
        Event event = Event.getEvent( message.data[ 0 ] );
        if( event == Event.MESSAGE_DENIED && Actions.getAction(message.data[ 2 ] ) == Actions.DELETE ) {
            return true;
        }

        Actions action = Actions.getAction( message.data[ 0 ] );

        return action == Actions.DELETE || action == Actions.UNSUBSCRIBE;
    }

    @ObjectiveCName("isUnhandledError:")
    private Boolean isUnhandledError(Message message) {
        if( message.action != Actions.ERROR ) {
            return false;
        }

        String errorType =  message.data[ 0 ];
        return !(errorType.equals(Event.VERSION_EXISTS.toString())
                || errorType.equals(Event.MESSAGE_DENIED.toString())
                || errorType.equals(Actions.SNAPSHOT.toString())
                || errorType.equals(Actions.HAS.toString()));

    }

    private class RecordHandlerListeners implements RecordEventsListener, Record.RecordDestroyPendingListener {
        /**
         * A collection of factories for records. This class
         * is exposed as client.record
         */
        RecordHandlerListeners() {
        }

        @Override
        @ObjectiveCName("onError:errorType:errorMessage:")
        public void onError(String recordName, Event errorType, String errorMessage) {
            client.onError(Topic.RECORD, errorType, recordName + ":" + errorMessage);
        }

        @Override
        @ObjectiveCName("onRecordHasProviderChanged:hasProvider:")
        public void onRecordHasProviderChanged(String recordName, boolean hasProvider) {

        }

        /**
         * When the client calls discard or delete on a record, there is a short delay
         * before the corresponding ACK message is received from the server. To avoid
         * race conditions if the record is re-requested straight away the old record is
         * removed from the cache straight awy and will only listen for one last ACK message
         */
        @Override
        @ObjectiveCName("onDestroyPending:")
        public void onDestroyPending(final String recordName) {
            //TODO:
            /*destroyEventEmitter.on( "destroy_ack_" + recordName, new Emitter.Listener() {
                @Override
                public void call(Object... args) {
                    Record record = records.get( recordName );
                    record.onMessage( (Message) args[ 0 ] );
                }
            } );*/
            onRecordDiscarded(recordName);
        }

        @Override
        @ObjectiveCName("onRecordDeleted:")
        public void onRecordDeleted(String recordName) {
            onRecordDiscarded(recordName);
        }

        @Override
        @ObjectiveCName("onRecordDiscarded:")
        public void onRecordDiscarded(String recordName) {
            records.remove(recordName);
            lists.remove(recordName);
        }
    }
}
