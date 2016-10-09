package io.deepstream;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

/**
 * Provides the different types of connections the deepstream client support
 */
public enum EndpointType {
    /**
     * TCP connection is deprecated
     */
    TCP("tcp"),
    /**
     * Websockets is the only type of transport supported going forward
     */
    WEBSOCKET("websocket");

    private static final Map<String, EndpointType> lookup = new HashMap<>();

    static {
        for (EndpointType s : EnumSet.allOf(EndpointType.class))
            lookup.put(s.toString(), s);
    }

    private String endpointType;

    EndpointType(String topic) {
        this.endpointType = topic;
    }

    static EndpointType getEndpointType(String endpointType) {
        return lookup.get(endpointType);
    }

    @Override
    public String toString() {
        return this.endpointType;
    }
}
