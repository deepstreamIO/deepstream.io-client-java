package io.deepstream;


import com.google.gson.Gson;
import io.deepstream.constants.Actions;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;
import javafx.util.Pair;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.Map;


public class Record extends Emitter {

    private UtilAckTimeoutRegistry ackTimeoutRegistry;
    private IConnection connection;
    IDeepstreamClient client;
    private String name;
    private Map recordOptions;
    private Map options;
    public int usages;
    public int version;
    Object record;
    private Object data;
    private String rawData;
    private Gson gson;
    Class clazz;
    UtilObjectDiffer differ;


    public Record(String name, Map recordOptions, IConnection connection, Map options, IDeepstreamClient client) {
        this.ackTimeoutRegistry = client.getAckTimeoutRegistry( client );
        this.name = name;
        this.recordOptions = recordOptions;
        this.options = options;
        this.usages = 0;
        this.version = 0;
        this.connection = connection;
        this.client = client;
        this.gson = new Gson();
        this.differ = new UtilObjectDiffer();
        this.data = null;
        this.rawData = null;
        this.scheduleAcks();
        this.sendRead();
    }

    public void set( Object obj ) {
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
    public Map get() {
        throw new NotImplementedException();
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
            this.data = gson.fromJson( this.rawData, clazz );
            return clone((T) this.data);
        }
        return null;
    }

    /**
     * Gets the given path of a record as a typed object
     *
     * @param path the path to get from the record
     * @param clazz type of class to deserialize the path into
     * @return a deserialized object of type T
     */
    public <T> T get( String path, Class<T> clazz ) {
        throw new NotImplementedException();
    }

    public void get( String path ) {
        throw new NotImplementedException();

    }

    public void onMessage( Message message ) {
        if( message.action == Actions.READ ) {
            if( this.version == 0 ) {
                this.ackTimeoutRegistry.clear( message );
                this.onRead( message );
            } else {
                this.applyUpdate( message, this.client );
            }
        }
        else if( message.action == Actions.ACK ) {
            this.processAckMessage( message );
        }

    }

    private void onRead( Message message ) {
        this.version = Integer.parseInt( message.data[ 1 ] );
        this.rawData = message.data[ 2 ];
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
        //this.resubscribeNotifier.destroy();
        //this.isDestroyed = true;
        //this.isReady = false;
        this.client = null;
        this.connection = null;
    }

    private <T> T clone(T data) {
        String serialized = gson.toJson( data );
        return (T) gson.fromJson(serialized, this.clazz);
    }
}
