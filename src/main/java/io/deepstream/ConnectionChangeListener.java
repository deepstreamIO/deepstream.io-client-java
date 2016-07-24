package io.deepstream;

import io.deepstream.constants.ConnectionState;

public interface ConnectionChangeListener {
    void connectionStateChanged(ConnectionState connectionState );
}
