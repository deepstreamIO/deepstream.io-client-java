package io.socket.engineio.client;

import io.socket.emitter.Emitter;

import java.util.ArrayList;

public class Socket extends Emitter {

    public static final String EVENT_OPEN = "open";
    public static final String EVENT_MESSAGE = "message";
    public static final String EVENT_ERROR = "error";

    public String url;
    public String lastSentMessage;
    public Boolean isDisconnected;
    public ArrayList<String> sentMessages;

    public Socket( String url ) {
        super();

        this.lastSentMessage = null;
        this.url = url;
        this.isDisconnected = true;
        this.sentMessages = new ArrayList<>();
    }

    public Socket open() {
        this.isDisconnected = false;
        this.emit( EVENT_OPEN );
        return this;
    }

    public void send( String message ) {
        this.lastSentMessage = message;
        this.sentMessages.add( message );
    }
}
