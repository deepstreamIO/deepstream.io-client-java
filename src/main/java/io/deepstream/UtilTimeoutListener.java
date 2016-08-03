package io.deepstream;

import io.deepstream.constants.Actions;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

interface UtilTimeoutListener {
    void onTimeout(Topic topic, Actions action, Event event, String name );
}
