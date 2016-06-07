package io.deepstream;

import io.deepstream.constants.Actions;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Topic;
import io.deepstream.message.MessageBuilder;

public class ConnectionMock implements IConnection {

    public String lastSentMessage;
    public ConnectionState state;

    public ConnectionMock() {
        this.lastSentMessage = null;
        this.state = ConnectionState.CLOSED;
    }

    public void sendMsg(Topic topic, Actions action, Object data) {
        this.lastSentMessage = MessageBuilder.getMsg( topic, action, data );
    }
}
