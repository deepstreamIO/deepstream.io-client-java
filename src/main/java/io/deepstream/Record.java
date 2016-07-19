package io.deepstream;


import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.deepstream.constants.Actions;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;
import javafx.util.Pair;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.Map;


public class Record extends Emitter implements UtilResubscribeCallback {

    public int usages;
    public int version;

    private final UtilResubscribeNotifier utilResubscribeNotifier;
    private final UtilAckTimeoutRegistry ackTimeoutRegistry;
    private final IConnection connection;
    private final IDeepstreamClient client;
    private final UtilObjectDiffer differ;
    private final Gson gson;

    private RecordEventsListener recordEventsListener;
    private JsonObject data;
    private String name;
    private Map options;
    private String rawData;
    private JSONPath path;
    private Class clazz;


    public Record(String name, Map recordOptions, IConnection connection, Map options, IDeepstreamClient client) {
        this.ackTimeoutRegistry = client.getAckTimeoutRegistry();
        this.utilResubscribeNotifier = new UtilResubscribeNotifier( client, this );
        this.name = name;
        this.options = options;
        this.usages = 0;
        this.version = 0;
        this.connection = connection;
        this.client = client;
        this.gson = new Gson();
        this.differ = new UtilObjectDiffer();
        this.data = new JsonObject();
        this.path = new JSONPath( this.data );
        this.scheduleAcks();
        this.sendRead();
    }

    public void setRecordEventsListener(RecordEventsListener recordEventsListener) {
        this.recordEventsListener = recordEventsListener;
    }

    public void set( Object value ) {
        this.data = (JsonObject) gson.toJsonTree( value );
        this.path = new JSONPath( this.data );
    }

    public void set( String path, Object value ) {
        this.path.set( path, gson.toJsonTree( value ) );
    }

    public void set( Class obj ) {
        this.version++;
        Pair<String, Object> pair = differ.getDiff(this.data, obj);

        if( pair.getKey().equals("") ) {
            this.connection.sendMsg( Topic.RECORD, Actions.UPDATE, new String[] {
                    this.name,
                    String.valueOf( this.version ),
                    gson.toJson( pair.getValue() )
            });
        }
        else {
            this.connection.sendMsg( Topic.RECORD, Actions.PATCH, new String[] {
                this.name,
                String.valueOf( this.version ),
                pair.getKey(),
                MessageBuilder.typed( pair.getValue() )
            });
        }
    }

    /**
     * Gets the record as a JSON object
     *
     * @return
     */
    public Object get( String path ) {
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

    public void delete() {
    }

    public void discard() {
    }

    protected void onMessage(Message message) {

    }

    private void onRead( Message message ) {
        this.version = Integer.parseInt( message.data[ 1 ] );
        this.data = gson.fromJson( message.data[ 2 ], JsonObject.class );
    }

    private void sendRead() {
        this.connection.sendMsg( Topic.RECORD, Actions.CREATEORREAD, new String[] { this.name } );
    }

    private void scheduleAcks() {
        int readAckTimeout = Integer.parseInt( (String) this.options.get( "recordReadAckTimeout" ) );
        this.ackTimeoutRegistry.add( Topic.RECORD, Actions.SUBSCRIBE, this.name, Event.ACK_TIMEOUT, readAckTimeout );

        int readResponseTimeout = Integer.parseInt( (String) this.options.get( "recordReadTimeout" ) );
        this.ackTimeoutRegistry.add( Topic.RECORD, Actions.READ, this.name, Event.RESPONSE_TIMEOUT, readResponseTimeout );
    }

    private void onTimeout( Event timeoutType ) {
        this.clearTimeouts();
        this.emit( "error", timeoutType );
    }

    private void clearTimeouts() {
        this.ackTimeoutRegistry.clear( Topic.RECORD, Actions.READ, this.name );
        this.ackTimeoutRegistry.clear( Topic.RECORD, Actions.READ, this.name );
    }

    private void applyUpdate(Message message, IDeepstreamClient client) {
        throw new NotImplementedException();
    }

    private void processAckMessage(Message message) {
        Actions action = Actions.getAction( message.data[ 0 ] );

        if( action.equals( Actions.SUBSCRIBE ) ) {
            this.ackTimeoutRegistry.clear( message );
        }
        else if( action.equals( Actions.DELETE ) ) {
            this.emit( "delete" );
            this.destroy();
        }
        else if( action.equals( Actions.UNSUBSCRIBE ) ) {
            this.emit( "discard" );
            this.destroy();
        }
    }

    private void destroy() {
        this.clearTimeouts();
        this.off();
        this.utilResubscribeNotifier.destroy();
//        this.isDestroyed = true;
//        this.isReady = false;
    }

    private <T> T clone(T data) {
        String serialized = gson.toJson( data );
        return (T) gson.fromJson(serialized, this.clazz);
    }

    @Override
    public void resubscribe() {
        sendRead();
    }
}
