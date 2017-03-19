//
//  IOSDeepstreamFactory.swift
//  DeepstreamIO
//
//  Created by Akram Hussein on 18/03/2017.
//  Copyright (c) 2017 deepstreamHub GmbH. All rights reserved.
//

import Foundation

/**
 * A singleton to stop you ( the developer ) from having to pass around the deepstream client reference
 * around the codebase, making application development easier in frameworks such android.
 * <p>
 * Currently this only contains a single deepstream client;
 */
public final class IOSDeepstreamFactory {

    private static let ourInstance = IOSDeepstreamFactory()
    private var clients = [String : DeepstreamClient]()
    private var lastUrl : String?

    public static func getInstance() -> IOSDeepstreamFactory {
        return ourInstance
    }

    /**
     * Returns the last client that was created. This is useful for most applications that
     * only require a single connection. The first time a client is connected however it has to be
     * via {@link DeepstreamFactory#getClient(String)} or {@link DeepstreamFactory#getClient(String, Properties)}
     *
     * @return A deepstream client
     */

    public func getClient() -> DeepstreamClient? {
        guard let lastUrl = self.lastUrl else {
            return nil
        }

        guard let client = self.clients[lastUrl] else {
            return nil
        }

        return client
    }

    /**
     * Returns a client that was previous created via the same url using this method or {@link DeepstreamFactory#getClient(String, Properties)}.
     * If one wasn't created, it creates it first and stores it for future reference.
     *
     * @param url The url to connect to, also the key used to retrieve in future calls
     * @return A deepstream client
     * @throws URISyntaxException An error if the url syntax is invalid
     */
    public func getClient(url: String) -> DeepstreamClient? {
        self.lastUrl = url

        // Check if client exists and not in closed/error state
        guard let c = self.clients[url], self.clientNotAvailable(client: c) else {
            let client = DeepstreamClient(url, endpointFactory: IOSEndpointWebsocketFactory())
            self.clients[url] = client
            return client
        }

        return c
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
    public func getClient(url: String, options: JavaUtilProperties) -> DeepstreamClient? {
        self.lastUrl = url

        // Check if client exists and not in closed/error state
        guard let c = self.clients[url], self.clientNotAvailable(client: c) else {
            let client = DeepstreamClient(url, options: options, endpointFactory: IOSEndpointWebsocketFactory())
            self.clients[url] = client
            return client
        }

        return c
    }

    private func clientNotAvailable(client: DeepstreamClient) -> Bool {
        let isClosed = (client.getConnectionState().toNSEnum() == ConnectionState_Enum.CLOSED)
        let isError = (client.getConnectionState().toNSEnum() == ConnectionState_Enum.ERROR)
        return isClosed || isError
    }
}
