package io.deepstream;

import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

public interface DeepstreamRuntimeErrorHandler {
    void onException(Topic topic, Event event, String msg);
}
