package io.deepstream;


import com.google.gson.JsonElement;
import io.deepstream.constants.Actions;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.util.HashMap;
import java.util.Map;

public class RecordHandler implements RecordEventsListener {

    private final Map options;
    private final IConnection connection;
    private final DeepstreamClientAbstract client;
    private final Map<String, Record> records;
    private final Map<String, List> lists;
    private final UtilSingleNotifier hasRegistry;
    private final UtilSingleNotifier snapshotRegistry;
    private final Map<String, UtilListener> listeners;

    /**
     * A collection of factories for records. This class
     * is exposed as client.record
     *
     * @param options The options the client was created with
     * @param connection The connection
     * @param client The deepstream client
     */
    RecordHandler( Map options, IConnection connection, DeepstreamClientAbstract client) {
        this.options = options;
        this.connection = connection;
        this.client = client;

        records = new HashMap<>();
        lists = new HashMap<>();
        listeners = new HashMap<>();

        int recordReadTimeout = Integer.parseInt( (String) options.get( "recordReadTimeout" ) );
        hasRegistry = new UtilSingleNotifier( client, connection, Topic.RECORD, Actions.SNAPSHOT, recordReadTimeout );
        snapshotRegistry = new UtilSingleNotifier( client, connection, Topic.RECORD, Actions.SNAPSHOT, recordReadTimeout );
    }

    /**
     * Returns an existing record or creates a new one. If creating a new one the record
     * will not be in a ready state till it is loaded from the server.
     * @param name The name of the record to get
     * @return Record The record
     */
    public Record getRecord( String name ) {
        Record record = records.get( name );
        if( record == null ) {
            record = new Record( name, new HashMap(), connection, options, client );
            records.put( name, record );
            record.addRecordEventsListener( this );
        }
        record.usages++;
        return record;
    }

    /**
     * Returns an existing List or creates a new one. A list is a specialised
     * type of record that holds an array of recordNames.
     *
     * @param name The name of the list to retrieve
     * @return List The List
     */
    public List getList( String name ) {
        List list = lists.get( name );
        if( list == null ) {
            list = new List( this, name, options );
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
     * Also worth mentioning that {@link AnonymousRecordReadyListener#onRecordReady(String, AnonymousRecord)}
     * can be called multiple times!
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
    public void listen( String pattern, ListenListener listenCallback ) {
        if( listeners.containsKey( pattern ) ) {
            // TODO: Do we really want to throw an error here?
            client.onError( Topic.RECORD, Event.LISTENER_EXISTS, pattern );
        } else {
            listeners.put( pattern, new UtilListener( Topic.RECORD, pattern, listenCallback, options, client, connection ));
        }
    }

    /**
     * Removes a listener that was previously registered with listenForSubscriptions
     *
     * @param pattern The pattern to stop listening to
     */
    public void unlisten( String pattern ) {
        UtilListener listener = listeners.get( pattern );
        if( listener != null ) {
            listener.destroy();
            listeners.remove( pattern );
        } else {
            // TODO: Do we really want to throw an error here?
            client.onError( Topic.RECORD, Event.NOT_LISTENING, pattern );
        }
    }

    /**
     * Retrieve the current record data without subscribing to changee<br/>
     *
     * If the record is loaded and ready the listener will be called sync, else
     * once the record is ready.<br/>
     *
     * If the record does not exist an error will be passed to {@link RecordSnapshotCallback#onRecordSnapshotError(String, DeepstreamException)}
     *
     * @param name The name of the record which state to retrieve
     * @param recordSnapshotCallback The callback of the successful/unsuccesful snapshot action
     */
    public void snapshot(String name, final RecordSnapshotCallback recordSnapshotCallback ) {
        Record record = records.get( name );
        if( record != null && record.isReady ) {
            recordSnapshotCallback.onRecordSnapshot( name, record.get() );
        } else {
            snapshotRegistry.request(name, new UtilSingleNotifierCallback() {
                @Override
                public void onSingleNotifierError(String name, DeepstreamException error) {
                    recordSnapshotCallback.onRecordSnapshotError( name, error );
                }

                @Override
                public void onSingleNotifierResponse(String name, Object data) {
                    recordSnapshotCallback.onRecordSnapshot(name, (JsonElement) data);
                }
            });
        }
    }

    /**
     * Allows the user to query to see whether or not the record exists<br/>
     *
     * If the record is created locally the listener will be called sync, else
     * once the record is ready.<br/>
     *
     * @param name The name of the record to check
     * @param callback The callback to indicate if the record exists
     */
    public void has(String name, final RecordHasCallback callback ) {
        Record record = records.get( name );
        if( record != null && record.isReady ) {
            callback.onRecordFound( name );
        } else {
            hasRegistry.request(name, new UtilSingleNotifierCallback() {
                @Override
                public void onSingleNotifierError(String name, DeepstreamException error) {
                    callback.onRecordHasError( name, error );
                }

                @Override
                public void onSingleNotifierResponse(String name, Object data) {
                    if( (boolean) data ) {
                        callback.onRecordFound( name );
                    } else {
                        callback.onRecordNotFound( name );
                    }
                }
            });
        }
    }


    /**
     * Will be called by the client for incoming messages on the RECORD topic
     */
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
                snapshotRegistry.recieve( recordName, new DeepstreamException( message.data[ 2 ] ), null );
                return;
            }

            if( message.data[ 0 ].equals(Actions.HAS.toString() ))  {
                hasRegistry.recieve( recordName, new DeepstreamException( message.data[ 2 ] ), null );
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
            client.onError( Topic.RECORD, Event.UNSOLICITED_MESSAGE, recordName );
        }
    }

    /**
     * The following methods checks to prevent errors that occur when a record is discarded or deleted and
     * recreated before the discard / delete ack message is received.
     *
     * A (presumably unsolvable) problem remains when a client deletes a record in the exact moment
     * between another clients creation and read message for the same record
     */
    private boolean isDiscardAck( Message message ) {
        Event event = Event.getEvent( message.data[ 0 ] );
        if( event == Event.MESSAGE_DENIED && Actions.getAction(message.data[ 2 ] ) == Actions.DELETE ) {
            return true;
        }

        Actions action = Actions.getAction( message.data[ 0 ] );

        if (action == Actions.DELETE) return true;
        if (action == Actions.UNSUBSCRIBE) return true;
        return false;
    }

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

    @Override
    public void onError(String recordName, Event errorType, String errorMessage) {
        client.onError( Topic.RECORD, errorType, recordName + ":" + errorMessage );
    }

    /**
     * When the client calls discard or delete on a record, there is a short delay
     * before the corresponding ACK message is received from the server. To avoid
     * race conditions if the record is re-requested straight away the old record is
     * removed from the cache straight awy and will only listen for one last ACK message
     */
    @Override
    public void onDestroyPending(final String recordName) {
        //TODO:
        /*destroyEventEmitter.on( "destroy_ack_" + recordName, new Emitter.Listener() {
            @Override
            public void call(Object... args) {
                Record record = records.get( recordName );
                record.onMessage( (Message) args[ 0 ] );
            }
        } );*/
        onRecordDiscarded( recordName );
    }

    @Override
    public void onRecordDeleted(String recordName) {
        onRecordDiscarded( recordName );
    }

    @Override
    public void onRecordDiscarded(String recordName) {
        records.remove( recordName );
        lists.remove( recordName );
    }
}
