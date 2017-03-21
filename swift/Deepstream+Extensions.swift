//
//  Deepstream+Extensions.swift
//  DeepstreamIO
//
//  Created by Akram Hussein on 17/12/2016.
//  Copyright (c) 2017 deepstreamHub GmbH. All rights reserved.
//

import Foundation
import Starscream

// MARK: - DeepstreamClient

public extension DeepstreamClient {
    public var record : RecordHandler {
        get {
            return self.getRecordHandler()
        }
    }

    public var event : EventHandler {
        get {
            return self.getEventHandler()
        }
    }

    public var rpc : RpcHandler {
        get {
            return self.getRpcHandler()
        }
    }

    public var presence : PresenceHandler {
        get {
            return self.getPresenceHandler()
        }
    }
}

// MARK: - JavaUtilProperties

public extension Dictionary where Key: ExpressibleByStringLiteral, Value: Any {
    public var toProperties : JavaUtilProperties {
        get {
            let properties = JavaUtilProperties()
            self.forEach { (key, value) in
                properties!.put(withId: key, withId: value)
            }
            return properties!
        }
    }
}

// MARK: - Foundation -> GSON

public extension Array where Element : Any {
    public var jsonElement : JsonArray {
        get {
            let data = try! JSONSerialization.data(withJSONObject: self, options: [])
            let json = String(data: data, encoding: String.Encoding.utf8)
            return Gson().fromJson(with: json, with: JsonArray_class_()) as! JsonArray
        }
    }
}

public extension Dictionary where Key: ExpressibleByStringLiteral {
    public var jsonElement : JsonObject {
        get {
            let data = try! JSONSerialization.data(withJSONObject: self, options: [])
            let json = String(data: data, encoding: String.Encoding.utf8)
            return Gson().fromJson(with: json, with: JsonObject_class_()) as! JsonObject
        }
    }
}

public extension IntegerLiteralType {
    public var jsonElement : JsonElement {
        get {
            let data = try! JSONSerialization.data(withJSONObject: self, options: [])
            let json = String(data: data, encoding: String.Encoding.utf8)
            return Gson().fromJson(with: json, with: JsonObject_class_()) as! JsonObject
        }
    }
}

public extension FloatingPoint {
    public var jsonElement : JsonElement {
        get {
            return Gson().toJsonTree(withId: self)
        }
    }
}

// MARK: - GSON -> Foundation

public extension JsonElement {
    static let gson = GsonBuilder().enableComplexMapKeySerialization().create()

    public var dict : [String : Any] {
        get {
            let serialized = JsonElement.gson?.toJson(with: self)
            let data = serialized!.data(using: .utf8)
            return try! JSONSerialization.jsonObject(with: data!, options: []) as! [String : Any]
        }
    }
}

public extension JsonArray {
    public var array : [Any] {
        get {
            let serialized = JsonElement.gson?.toJson(with: self)
            let data = serialized!.data(using: .utf8)
            return try! JSONSerialization.jsonObject(with: data!, options: []) as! [Any]
        }
    }
}

// MARK: - IOSDeepstreamFactory

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
    public func getClient(_ url: String, callback: @escaping (DeepstreamClient?) -> Void) {
        self.lastUrl = url

        DispatchQueue.global().async {
            // Check if client exists and not in closed/error state
            guard let c = self.clients[url], self.clientNotAvailable(client: c) else {
                let client = DeepstreamClient(url, endpointFactory: IOSEndpointWebsocketFactory())
                self.clients[url] = client
                return callback(client)
            }

            callback(c)
        }
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
    public func getClient(_ url: String, options: JavaUtilProperties, callback: @escaping (DeepstreamClient?) -> Void) {
        self.lastUrl = url

        DispatchQueue.global().async {
            // Check if client exists and not in closed/error state
            guard let c = self.clients[url], self.clientNotAvailable(client: c) else {
                let client = DeepstreamClient(url, options: options, endpointFactory: IOSEndpointWebsocketFactory())
                self.clients[url] = client
                return callback(client)
            }

            callback(c)
        }
    }

    private func clientNotAvailable(client: DeepstreamClient) -> Bool {
        let isClosed = (client.getConnectionState().toNSEnum() == ConnectionState_Enum.CLOSED)
        let isError = (client.getConnectionState().toNSEnum() == ConnectionState_Enum.ERROR)
        return isClosed || isError
    }
}

// MARK: - IOSEndpointWebsocket

private final class IOSEndpointWebsocket : NSObject, Endpoint {

    private let uri : JavaNetURI!
    public let connection : Connection!
    private var webSocket : WebSocket? {
        didSet {
            self.webSocket?.onConnect = {
                self.connection.onOpen()
            }

            self.webSocket?.onDisconnect = { (error: NSError?) in
                self.connection.onClose()
            }

            self.webSocket?.onText = { (text: String) in
                self.connection.onMessage(text)
            }

            self.webSocket?.onData = { (data: Data) in
            }

        }
    }

    public init(uri: JavaNetURI, connection: Connection) {
        self.uri = uri
        self.connection = connection
    }

    public func send(_ message: String!) {
        self.webSocket?.write(string: message)
    }

    public func close() {
        self.webSocket?.disconnect()
        self.webSocket = nil
    }

    public func forceClose() {
        self.webSocket?.disconnect(forceTimeout: nil, closeCode: 1)
    }

    public func open() {
        guard let url = URL(string: self.uri.toASCIIString()) else {
            return
        }

        self.webSocket = WebSocket(url: url)
        self.webSocket?.connect()
    }
}

// MARK: - IOSEndpointWebsocketFactory

private final class IOSEndpointWebsocketFactory : NSObject, EndpointFactory {
    public func createEndpoint(_ uri: JavaNetURI!, connection: Connection!) -> Endpoint! {
        return IOSEndpointWebsocket(uri: uri, connection: connection)
    }
}
