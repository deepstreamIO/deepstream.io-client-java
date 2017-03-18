package io.deepstream;

import java.net.URI;
import java.net.URISyntaxException;

class JavaEndpointFactory implements EndpointFactory {

    @Override
    public Endpoint createEndpoint(URI uri, Connection connection) throws URISyntaxException {
        return new JavaEndpointWebsocket(uri, connection);
    }
}
