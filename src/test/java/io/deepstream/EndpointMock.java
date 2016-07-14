package io.deepstream;
import java.util.ArrayList;

public class EndpointMock implements Endpoint {

    public static final String EVENT_OPEN = "open";
    public static final String EVENT_MESSAGE = "message";
    public static final String EVENT_ERROR = "error";
    private Connection connection;

    public String url;
    public String lastSentMessage;
    public Boolean isDisconnected;
    public ArrayList<String> sentMessages;

    public EndpointMock( String url, Connection connection ) {
        this.connection = connection;
        this.lastSentMessage = null;
        this.url = url;
        this.isDisconnected = true;
        this.sentMessages = new ArrayList<String>();
    }

    public void setConnection( Connection connection ) {
        this.connection = connection;
    }

    public void sendOpenEvent() {
        this.connection.onOpen();
    }

    public void sendMessage( String message ) {
        try {
            this.connection.onMessage( message );
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void sendError( Exception exception ) {
        this.connection.onError( exception.getMessage() );
    }

    public void send( String message ) {
        this.lastSentMessage = message;
        this.sentMessages.add( message );
    }

    @Override
    public void close() {

    }

    @Override
    public void open() {

    }
}
