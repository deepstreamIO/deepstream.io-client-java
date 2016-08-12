package io.deepstream;

import com.google.gson.JsonElement;

abstract class DeepstreamClientAbstract {
    private UtilAckTimeoutRegistry utilAckTimeoutRegistry;
    private DeepstreamRuntimeErrorHandler deepstreamRuntimeErrorHandler;

    abstract DeepstreamClientAbstract addConnectionChangeListener(ConnectionStateListener connectionStateListener);
    abstract DeepstreamClientAbstract removeConnectionChangeListener(ConnectionStateListener connectionStateListener);
    abstract ConnectionState getConnectionState();

    abstract LoginResult login() throws DeepstreamLoginException;

    abstract LoginResult login(JsonElement data) throws DeepstreamLoginException;
    abstract DeepstreamClientAbstract close();
    abstract String getUid();

    UtilAckTimeoutRegistry getAckTimeoutRegistry() {
        if(  utilAckTimeoutRegistry == null ) {
            utilAckTimeoutRegistry = new UtilAckTimeoutRegistry( this );
        }
        return utilAckTimeoutRegistry;
    }

    /**
     * Adds a {@link DeepstreamRuntimeErrorHandler} that will catch all RuntimeErrors such as AckTimeouts and allow
     * the user to gracefully handle them.
     *
     * @param deepstreamRuntimeErrorHandler The listener to set
     */
    public void setRuntimeErrorHandler(DeepstreamRuntimeErrorHandler deepstreamRuntimeErrorHandler) {
        this.deepstreamRuntimeErrorHandler = deepstreamRuntimeErrorHandler;
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
            throw new DeepstreamException( topic, event, msg );
        }

    }
}
