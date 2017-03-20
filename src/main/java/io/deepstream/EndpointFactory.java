package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import java.net.URI;
import java.net.URISyntaxException;

interface EndpointFactory {
    @ObjectiveCName("createEndpoint:connection:")
    public Endpoint createEndpoint(URI uri, Connection connection) throws URISyntaxException;
}
