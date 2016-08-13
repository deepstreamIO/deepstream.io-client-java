package io.deepstream;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

public enum EndpointType {
    /**
     * TCP connection is used for connections with large amounts of data and no
     * requirement for websocket/http protocol
     */
    TCP("tcp"),
    /**
     * EngineIO is to be used for connections that have multiple firewalls ( with access only to http ports )
     * and for fallbacks to http polling mechanisms
     */
    ENGINEIO("engineio");

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
