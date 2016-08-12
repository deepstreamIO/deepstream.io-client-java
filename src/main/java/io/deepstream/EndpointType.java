package io.deepstream;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

public enum EndpointType {
    TCP("tcp"),
    ENGINEIO("engineio"),
    MOCK("mock");

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
