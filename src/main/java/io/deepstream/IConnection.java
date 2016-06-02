package io.deepstream;

import io.deepstream.constants.Actions;
import io.deepstream.constants.Topic;

/**
 * Created by Alex on 2/06/2016.
 */
public interface IConnection {

    void sendMsg( Topic topic, Actions action, String data );
}
