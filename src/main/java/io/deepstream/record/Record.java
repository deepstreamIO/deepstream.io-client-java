package io.deepstream.record;


import com.google.gson.Gson;
import io.deepstream.IConnection;
import io.deepstream.IDeepstreamClient;
import io.deepstream.constants.Actions;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;
import io.deepstream.message.Message;
import io.deepstream.utils.Emitter;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

public class Record<T> extends Emitter {

    private IConnection connection;
    IDeepstreamClient client;
    private String name;
    private Map recordOptions;
    private Map options;
    public int usages;
    public int version;
    private String rawData;
    private Gson gson;
    Class<T> clazz;
    private TimerTask readAckTimeout;
    private TimerTask readTimeout;


    public Record(String name, Map recordOptions, IConnection connection, Map options, IDeepstreamClient client) {
        this.name = name;
        this.recordOptions = recordOptions;
        this.options = options;
        this.usages = 0;
        this.version = 0;
        this.connection = connection;
        this.client = client;
        this.gson = new Gson();
        this.rawData = null;
        this.scheduleAcks();
        this.sendRead();
    }

    public void set( T obj ) {
        this.version++;
        this.rawData = gson.toJson( obj );
        this.connection.sendMsg( Topic.RECORD, Actions.UPDATE, new String[] {
                this.name,
                String.valueOf( this.version ),
                this.rawData
        });
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
    public T get( Class<T> clazz ) {
        if( this.rawData != null ) {
            this.clazz = clazz;
            return gson.fromJson( this.rawData, this.clazz );
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
    public T get( String path, Class<T> clazz ) {
        throw new NotImplementedException();
    }

    public void get( String path ) {
        throw new NotImplementedException();

    }

    public void onMessage( Message message ) {
        if( message.action == Actions.READ ) {
            if( this.version == 0 ) {
                this.readTimeout.cancel();
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
        Timer timer = new Timer();
        this.readAckTimeout = new TimerTask() {
            public void run() {
                onTimeout( Event.ACK_TIMEOUT );
            }
        };
        this.readTimeout = new TimerTask() {
            public void run() {
                onTimeout( Event.RESPONSE_TIMEOUT );
            }
        };
        
        int readAckTimeout = Integer.parseInt( (String) this.options.get( "recordReadAckTimeout" ) );
        int readResponseTimeout = Integer.parseInt( (String) this.options.get( "recordReadTimeout" ) );
        timer.schedule( this.readAckTimeout, readAckTimeout );
        timer.schedule( this.readTimeout, readResponseTimeout );
    }

    private void onTimeout( Event timeoutType ) {
        this.clearTimeouts();
        this.emit( "error", timeoutType );
    }

    private void clearTimeouts() {
        this.readAckTimeout.cancel();
        this.readTimeout.cancel();
    }

    private void applyUpdate(Message message, IDeepstreamClient client) {
        throw new NotImplementedException();
    }

    private void processAckMessage(Message message) {
        Actions action = Actions.getAction( message.data[ 0 ] );

        if( action.equals( Actions.SUBSCRIBE ) ) {
            this.readAckTimeout.cancel();
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
}
