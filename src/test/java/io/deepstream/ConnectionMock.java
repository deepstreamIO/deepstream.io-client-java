package io.deepstream;

import io.deepstream.constants.Actions;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Topic;

public class ConnectionMock implements IConnection {

    public String lastSentMessage;
    public ConnectionState state;

    public ConnectionMock() {
        this.lastSentMessage = null;
        this.state = ConnectionState.CLOSED;
    }

    @Override
    public void sendMsg(Topic topic, Actions action, String[] data) {
        this.lastSentMessage = MessageBuilder.getMsg( topic, action, data );
    }

    @Override
    public void send(String message) {
        this.lastSentMessage = message;
    }
}
