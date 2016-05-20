package io.deepstream;

import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;

public interface ErrorCallback {
        void onError(Topic topic, Event event, String message );
}
