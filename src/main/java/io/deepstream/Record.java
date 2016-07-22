package io.deepstream;


import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.deepstream.constants.Actions;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;
import javafx.util.Pair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;


public class Record extends Emitter implements UtilResubscribeCallback {

    public static String DESTROY_PENDING = "DESTROY_PENDING";

    public int usages;
    public int version;

    public boolean isReady;
    public boolean isDestroyed;

    private final UtilResubscribeNotifier utilResubscribeNotifier;
    private final UtilAckTimeoutRegistry ackTimeoutRegistry;
    private final IConnection connection;
    private final IDeepstreamClient client;
    private final UtilObjectDiffer differ;
    private final Gson gson;

    private RecordEventsListener recordEventsListener;
    private RecordMergeStrategy mergeStrategy;
    private JSONPath path;
    private Map<String, ArrayList> subscribers;

    private Class clazz;

    private JsonElement data;
    private String name;
    private Map options;
    private String rawData;

    public Record(String name, Map recordOptions, IConnection connection, Map options, IDeepstreamClient client) {
        this.ackTimeoutRegistry = client.getAckTimeoutRegistry();
        this.utilResubscribeNotifier = new UtilResubscribeNotifier( client, this );
        this.name = name;
        this.options = options;
        this.usages = 0;
        this.version = -1;
        this.connection = connection;
        this.client = client;
        this.gson = new Gson();
        this.differ = new UtilObjectDiffer();
        this.data = new JsonObject();
        this.path = new JSONPath( this.data );
        this.subscribers = new HashMap<>();
        this.isReady = false;
        this.isDestroyed = false;
        this.scheduleAcks();
        this.sendRead();
    }

    public Record setRecordEventsListener(RecordEventsListener recordEventsListener) {
        this.recordEventsListener = recordEventsListener;
        return this;
    }

    public Record setMergeStrategy(RecordMergeStrategy mergeStrategy) {
        this.mergeStrategy = mergeStrategy;
        return this;
    }

    /**
     * Gets the record as a JSON object
     *
     * @return
     */
    public JsonElement get( String path ) {
        return this.path.get( path );
    }

    /**
     * Gets the record as a JSON object
     *
     * @return
     */
    public JsonElement get() {
        return this.data;
    }

    public Record set( Object value ) throws DeepstreamRecordDestroyedException {
        this.set( null, value );
        return this;
    }

    public Record set(String path, Object value ) throws DeepstreamRecordDestroyedException {
        throwExceptionIfDestroyed( "set" );

        JsonElement element = gson.toJsonTree( value );

        if( !this.isReady && path != null ) {
            System.out.println( "Not ready, should queue!" );
            return this;
        }

        if ( this.data.equals( value ) ) {
            return this;
        }

        beginChange();
        this.version++;
        this.path.set( path, element );
        this.data = this.path.getCoreElement();
        sendUpdate( path, value );
        completeChange();

        return this;
    }

    public Record subscribe( String path, RecordChangedCallback recordChangedCallback ) throws DeepstreamRecordDestroyedException {
        throwExceptionIfDestroyed( "subscribe" );
        return this;
    }

    public Record subscribe( RecordChangedCallback recordChangedCallback ) throws DeepstreamRecordDestroyedException {
        subscribe( null, recordChangedCallback );
        return this;
    }

    public Record unsubscribe( String path, RecordChangedCallback recordChangedCallback ) throws DeepstreamRecordDestroyedException {
        throwExceptionIfDestroyed( "unsubscribe" );
        return this;
    }

    public Record unsubscribe( RecordChangedCallback recordChangedCallback ) throws DeepstreamRecordDestroyedException {
        subscribe( null, recordChangedCallback );
        return this;
    }

    public Record discard() throws DeepstreamRecordDestroyedException {
        throwExceptionIfDestroyed( "delete" );
        this.usages--;
        if( this.usages <= 0 ) {
            this.emit(DESTROY_PENDING);
            // TODO: on ready async callback
            int subscriptionTimeout = Integer.parseInt( (String) this.options.get( "subscriptionTimeout" ) );
            this.ackTimeoutRegistry.add( Topic.RECORD, Actions.UNSUBSCRIBE, name, subscriptionTimeout );
            this.connection.send( MessageBuilder.getMsg( Topic.RECORD, Actions.UNSUBSCRIBE, this.name ) );
        }
        return this;
    }

    public Record delete() throws DeepstreamRecordDestroyedException {
        throwExceptionIfDestroyed( "delete" );
        this.emit(DESTROY_PENDING);

        // TODO: on ready async callback
        int subscriptionTimeout = Integer.parseInt( (String) this.options.get( "recordDeleteTimeout" ) );
        this.ackTimeoutRegistry.add( Topic.RECORD, Actions.DELETE, name, Event.DELETE_TIMEOUT, subscriptionTimeout );
        this.connection.send( MessageBuilder.getMsg( Topic.RECORD, Actions.DELETE, this.name ) );

        return this;
    }

    public Record whenReady() {
        // TODO: Architectural Decision, how do we want async to work?!
        return this;
    }

    protected void onMessage(Message message) {
        if( message.action == Actions.ACK ) {
            processAckMessage( message );
        } else if( message.action == Actions.READ ) {
            onRead( message );
        } else if( message.action == Actions.UPDATE || message.action == Actions.PATCH ) {
            applyUpdate( message );
        } else if( message.data[ 0 ] == Event.VERSION_EXISTS.toString() ) {
            recoverRecord( Integer.parseInt( message.data[ 2 ] ), gson.fromJson( message.data[ 3 ], JsonElement.class ), message );
        } else if( message.data[ 0 ] == Event.MESSAGE_DENIED.toString() ) {
           clearTimeouts();
        }
    }

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

        beginChange();

        this.version = version;
        if( Actions.PATCH == message.action ) {
            path.set( message.data[ 2 ], data );
        } else {
            this.data = data;
            this.path.setCoreElement( data );
        }

        completeChange();

    }

    private void recoverRecord(int version, Object data, Message message) {
        // TODO: Apply merge strategy
    }

    private void scheduleAcks() {
        int readAckTimeout = Integer.parseInt( (String) this.options.get( "recordReadAckTimeout" ) );
        this.ackTimeoutRegistry.add( Topic.RECORD, Actions.SUBSCRIBE, this.name, Event.ACK_TIMEOUT, readAckTimeout );

        int readResponseTimeout = Integer.parseInt( (String) this.options.get( "recordReadTimeout" ) );
        this.ackTimeoutRegistry.add( Topic.RECORD, Actions.READ, this.name, Event.RESPONSE_TIMEOUT, readResponseTimeout );
    }

    private void clearTimeouts() {
        this.ackTimeoutRegistry.clear( Topic.RECORD, Actions.SUBSCRIBE, this.name );
        this.ackTimeoutRegistry.clear( Topic.RECORD, Actions.READ, this.name );
    }

    private void beginChange() {
        if( this.subscribers.isEmpty() ) {
            return;
        }
    }

    private void completeChange() {
    }

    private void throwExceptionIfDestroyed(String action) throws DeepstreamRecordDestroyedException {
        if( this.isDestroyed ) {
            throw new DeepstreamRecordDestroyedException( );    
        }
    }

    private void processAckMessage(Message message) {
        Actions action = Actions.getAction( message.data[ 0 ] );
        this.ackTimeoutRegistry.clear( message );

        if( action.equals( Actions.DELETE ) ) {
            this.emit( "delete" );
            if( this.recordEventsListener != null ) {
                recordEventsListener.onRecordDeleted( this.name );
            }
            this.destroy();
        }
        else if( action.equals( Actions.UNSUBSCRIBE ) ) {
            this.emit( "discard" );
            if( this.recordEventsListener != null ) {
                recordEventsListener.onRecordDiscarded( this.name );
            }
            this.destroy();
        }
    }

    private void onRead( Message message ) {
        ackTimeoutRegistry.clear( message );

        beginChange();
        this.version = Integer.parseInt( message.data[ 1 ] );
        this.data = gson.fromJson( message.data[ 2 ], JsonObject.class );
        completeChange();
        setReady();
    }

    private void setReady() {
        this.isReady = true;
        //TODO: Emit ready events
        this.emit( "ready" );
        if( this.recordEventsListener != null ) {
            recordEventsListener.onRecordReady( this.name );
        }
    }

    private void sendRead() {
        if( this.client.getConnectionState() == ConnectionState.OPEN ) {
            this.connection.send( MessageBuilder.getMsg( Topic.RECORD, Actions.CREATEORREAD, this.name ) );
        }
    }

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
    @Override
    public void resubscribe() {
        sendRead();
    }

    private void destroy() {
        this.clearTimeouts();
        this.off();
        this.utilResubscribeNotifier.destroy();
        this.isReady = false;
        this.isDestroyed = true;
    }

    /**
     * Class API
     */
    public void set( Class obj ) {
        this.version++;
        Pair<String, Object> pair = differ.getDiff(this.data, obj);
        sendUpdate( pair.getKey(), pair.getValue() );
    }

    private <T> T clone(T data) {
        String serialized = gson.toJson( data );
        return (T) gson.fromJson(serialized, this.clazz);
    }

    /**
     * Gets the record as a typed object
     *
     * @param clazz type of class to deserialize json into
     * @return a deserialized object of type T
     */
    public <T> T get( Class<T> clazz ) {
        if( this.rawData != null ) {
            this.clazz = clazz;
            return clone((T) gson.fromJson( this.rawData, clazz ));
        }
        return null;
    }
}
