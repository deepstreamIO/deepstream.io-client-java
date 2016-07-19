package io.deepstream;


import io.deepstream.constants.Actions;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

import java.util.HashMap;
import java.util.Map;

public class RecordHandler implements RecordEventsListener {

    private final Map options;
    private final IConnection connection;
    private final IDeepstreamClient client;
    private final Map<String, Record> records;
    private final Map<String, List> lists;
    private final UtilSingleNotifier hasRegistry;
    private final UtilSingleNotifier snapshotRegistry;
    private final Map<String, UtilListener> listeners;
    private final Emitter destroyEventEmitter;

    /**
     * A collection of factories for records. This class
     * is exposed as client.record
     *
     * @param options
     * @param connection
     * @param client
     */
    public RecordHandler( Map options, IConnection connection, IDeepstreamClient client) {
        this.options = options;
        this.connection = connection;
        this.client = client;

        records = new HashMap<>();
        lists = new HashMap<>();
        listeners = new HashMap<>();

        int recordReadTimeout = Integer.parseInt( (String) options.get( "recordReadTimeout" ) );
        hasRegistry = new UtilSingleNotifier( client, connection, Topic.RECORD, Actions.SNAPSHOT, recordReadTimeout );
        snapshotRegistry = new UtilSingleNotifier( client, connection, Topic.RECORD, Actions.SNAPSHOT, recordReadTimeout );

        destroyEventEmitter = new Emitter();
    }

    /**
     * Returns an existing record or creates a new one
     * @param name
     * @return
     */
    public Record getRecord( String name ) {
        Record record = records.get( name );
        if( record == null ) {
            record = new Record( name, new HashMap(), connection, options, client );
            records.put( name, record );
            record.setRecordEventsListener( this );
        }
        record.usages++;
        return record;
    }

    /**
     * Returns an existing List or creates a new one. A list is a specialised
     * type of record that holds an array of recordNames.
     *
     * @param name
     * @return
     */
    public List getList( String name ) {
        return new List( this, name, options );
    }

    /**
     * Returns an anonymous record. A anonymous record is effectively
     * a wrapper that mimicks the API of a record, but allows for the
     * underlying record to be swapped without loosing subscriptions etc.
     *
     * This is particularly useful when selecting from a number of similarly
     * structured records. E.g. a list of users that can be choosen from a list
     *
     * The only API difference to a normal record is an additional setName( name ) method.
     *
     * @param name
     * @return
     */
    public AnonymousRecord getAnonymousRecord( String name ) {
        return new AnonymousRecord( this );
    }

    /**
     * Allows to listen for record subscriptions made by this or other clients. This
     * is useful to create "active" data providers, e.g. providers that only provide
     * data for a particular record if a user is actually interested in it
     *
     * @param pattern
     * @param listenCallback
     */
    public void listen( String pattern, ListenCallback listenCallback ) {
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
     * @param pattern
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
     * Retrieve the current record data without subscribing to changes
     *
     * @param name
     */
    public void snapshot( String name, SingleNotifierCallback callback ) {
        Record record = records.get( name );
        if( record != null ) {
            callback.onSingleNotifierResponse( name, record.get() );
        } else {
            snapshotRegistry.request( name, callback );
        }
    }

    /**
     *  Allows the user to query to see whether or not the record exists
     *
     * @param name
     * @param callback
     */
    public void has( String name, SingleNotifierCallback callback ) {
        Record record = records.get( name );
        if( record != null ) {
            callback.onSingleNotifierResponse( name, true );
        } else {
            snapshotRegistry.request( name, callback );
        }
    }


    /**
     * Will be called by the client for incoming messages on the RECORD topic
     * @param message
     */
    public void handle( Message message ) {
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
                destroyEventEmitter.emit( "destroy_ack_" + recordName, message );

                record = records.get( recordName );
                if( Actions.getAction( message.data[ 0 ] ) == Actions.DELETE && record != null ) {
                    record.onMessage( message );
                }

                return;
            }

            if( message.data[ 0 ] == Actions.SNAPSHOT.toString() ) {
                snapshotRegistry.recieve( recordName, message.data[ 2 ], null );
                return;
            }

            if( message.data[ 0 ] == Actions.HAS.toString() ) {
                snapshotRegistry.recieve( recordName, message.data[ 2 ], null );
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
     * @param message
     * @return
     */
    private boolean isDiscardAck( Message message ) {
        Event event = Event.getEvent( message.data[ 0 ] );
        if( event == Event.MESSAGE_DENIED && Actions.getAction(message.data[ 2 ] ) == Actions.DELETE ) {
            return true;
        }

        Actions action = Actions.getAction( message.data[ 0 ] );
        if( action == Actions.DELETE || action == Actions.UNSUBSCRIBE ) {
            return true;
        }

        return false;
    }

    /**
     * @param message
     * @return
     */
    private Boolean isUnhandledError(Message message) {
        if( message.action != Actions.ERROR ) {
            return false;
        }

        String errorType =  message.data[ 0 ];
        if( errorType == Event.VERSION_EXISTS.toString()
                || errorType == Event.MESSAGE_DENIED.toString()
                || errorType == Actions.SNAPSHOT.toString()
                || errorType == Actions.HAS.toString() ) {
            return false;
        }

        return true;
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
        destroyEventEmitter.on( "destroy_ack_" + recordName, new Emitter.Listener() {
            @Override
            public void call(Object... args) {
                Record record = records.get( recordName );
                record.onMessage( (Message) args[ 0 ] );
            }
        } );
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
