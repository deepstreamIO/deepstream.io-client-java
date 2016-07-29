package io.deepstream;

import com.google.gson.JsonElement;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

abstract class IDeepstreamClient {
    private UtilAckTimeoutRegistry utilAckTimeoutRegistry;
    private DeepstreamRuntimeErrorHandler deepstreamRuntimeErrorHandler;

    abstract IDeepstreamClient addConnectionChangeListener(ConnectionChangeListener connectionChangeListener);
    abstract IDeepstreamClient removeConnectionChangeListener( ConnectionChangeListener connectionChangeListener );
    abstract ConnectionState getConnectionState();
    abstract IDeepstreamClient login( JsonElement data )throws DeepstreamLoginException ;
    abstract IDeepstreamClient login( JsonElement data, LoginCallback loginCallback ) throws DeepstreamLoginException ;
    abstract IDeepstreamClient close();
    abstract String getUid();

    UtilAckTimeoutRegistry getAckTimeoutRegistry() {
        if(  utilAckTimeoutRegistry == null ) {
            utilAckTimeoutRegistry = new UtilAckTimeoutRegistry( this );
        }
        return utilAckTimeoutRegistry;
    }

    public IDeepstreamClient setRuntimeErrorHandler( DeepstreamRuntimeErrorHandler deepstreamRuntimeErrorHandler )  {
        this.deepstreamRuntimeErrorHandler = deepstreamRuntimeErrorHandler;
        return this;
    }

    void onError(Topic topic, Event event, String msg) throws DeepstreamException {
        /*
         * Help to diagnose the problem quicker by checking for
         * some mon problems
         */
        if( event.equals( Event.ACK_TIMEOUT ) || event.equals( Event.RESPONSE_TIMEOUT ) ) {
            if( getConnectionState().equals( ConnectionState.AWAITING_AUTHENTICATION ) ) {
                String errMsg = "Your message timed out because you\'re not authenticated. Have you called login()?";
                onError( Topic.ERROR, Event.NOT_AUTHENTICATED, errMsg );
                return;
            }
        }

        if( deepstreamRuntimeErrorHandler != null ) {
            deepstreamRuntimeErrorHandler.onException( topic, event, msg );
        } else {
            System.out.println( "Throwing a client exception: " + topic + " " +  event + " " +  msg );
            throw new DeepstreamException( topic, event, msg );
        }

    }
}
