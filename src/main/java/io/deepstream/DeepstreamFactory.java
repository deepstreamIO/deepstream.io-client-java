package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 * A singleton to stop you ( the developer ) from having to pass around the deepstream client reference
 * around the codebase, making application development easier in frameworks such android.
 * <p>
 * Currently this only contains a single deepstream client;
 */
public class DeepstreamFactory {
    private static DeepstreamFactory ourInstance = new DeepstreamFactory();
    Map<String, DeepstreamClient> clients;
    String lastUrl;

    /**
     * DeepstreamFactory is a map of all url connections created
     */
    private DeepstreamFactory() {
        this.clients = new HashMap();
        this.lastUrl = null;
    }

    public static DeepstreamFactory getInstance() {
        return ourInstance;
    }

    /**
     * Returns the last client that was created. This is useful for most applications that
     * only require a single connection. The first time a client is connected however it has to be
     * via {@link DeepstreamFactory#getClient(String)} or {@link DeepstreamFactory#getClient(String, Properties)}
     *
     * @return A deepstream client
     */
    public DeepstreamClient getClient() {
        if (this.lastUrl == null) {
            return null;
        }
        return this.clients.get(this.lastUrl);
    }

    /**
     * Returns a client that was previous created via the same url using this method or {@link DeepstreamFactory#getClient(String, Properties)}.
     * If one wasn't created, it creates it first and stores it for future reference.
     *
     * @param url The url to connect to, also the key used to retrieve in future calls
     * @return A deepstream client
     * @throws URISyntaxException An error if the url syntax is invalid
     */
    @ObjectiveCName("getClient:")
    public DeepstreamClient getClient(String url) throws URISyntaxException {
        DeepstreamClient client = this.clients.get(url);
        this.lastUrl = url;
        if (clientDoesNotExist(client)) {
            client = new DeepstreamClient(url,  new DeepstreamConfig(), new JavaEndpointFactory());
            this.clients.put(url, client);
        }
        return client;
    }

    /**
     * Returns a client that was previous created via the same url using this method or {@link DeepstreamFactory#getClient(String, Properties)}.
     * If one wasn't created, it creates it first and stores it for future reference.
     *
     * @param url The url to connect to, also the key used to retrieve in future calls
     * @param networkAvailable boolean to indicate whether network is available or not
     * @return A deepstream client
     * @throws URISyntaxException An error if the url syntax is invalid
     */
    public DeepstreamClient getClient(String url, boolean networkAvailable) throws URISyntaxException {
        DeepstreamClient client = this.clients.get(url);
        this.lastUrl = url;
        if (clientDoesNotExist(client)) {
            client = new DeepstreamClient(url,  new DeepstreamConfig(), new JavaEndpointFactory(), networkAvailable);
            this.clients.put(url, client);
        }else{
            client.setGlobalConnectivityState(networkAvailable ? GlobalConnectivityState.CONNECTED : GlobalConnectivityState.DISCONNECTED);
        }
        return client;
    }

    /**
     * Returns a client that was previous created via the same url using this method or {@link DeepstreamFactory#getClient(String)}.
     * If one wasn't created, it creates it first and stores it for future reference.
     *
     * @param url     The url to connect to, also the key used to retrieve in future calls
     * @param options The options to use within the deepstream connection
     * @return A deepstream client
     * @throws URISyntaxException      An error if the url syntax is invalid
     * @throws InvalidDeepstreamConfig An exception if any of the options are invalid
     */
    @ObjectiveCName("getClient:options:")
    public DeepstreamClient getClient(String url, Properties options) throws URISyntaxException, InvalidDeepstreamConfig {
        DeepstreamClient client = this.clients.get(url);
        if (clientDoesNotExist(client)) {
            client = new DeepstreamClient(url, new DeepstreamConfig(options), new JavaEndpointFactory());
            this.clients.put(url, client);
        }
        return client;
    }

    @ObjectiveCName("clientDoesNotExist:")
    private boolean clientDoesNotExist(DeepstreamClient client) {
        return client == null || client.getConnectionState() == ConnectionState.CLOSED || client.getConnectionState() == ConnectionState.ERROR;
    }
}
