package io.deepstream;

import java.net.URI;
import java.net.URISyntaxException;

interface EndpointFactory {
    public Endpoint createEndpoint(URI uri, Connection connection) throws URISyntaxException;
}
