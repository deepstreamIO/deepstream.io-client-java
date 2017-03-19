//
//  iOSEndpoint.swift
//  DeepstreamIO
//
//  Created by Akram Hussein on 18/03/2017.
//  Copyright (c) 2017 deepstreamHub GmbH. All rights reserved.
//

import Starscream

public final class IOSEndpointWebsocket : NSObject, Endpoint {

    private let uri : JavaNetURI!
    public let connection : Connection!
    private var webSocket : WebSocket? {
        didSet {
            self.webSocket?.onConnect = {
                print("Websocket: websocketDidConnect")
                self.connection.onOpen()
            }

            self.webSocket?.onDisconnect = { (error: NSError?) in
                print("Websocket: websocketDidDisconnect")
                if (error != nil) {
                    print("Websocket: \(error?.localizedDescription)")
                    self.connection.onError("error")
                    return
                }
                self.connection.onClose()
            }

            self.webSocket?.onText = { (text: String) in
                print("Websocket: websocketDidReceiveMessage, \(text)")
                self.connection.onMessage(text)
            }

            self.webSocket?.onData = { (data: Data) in
                print("Websocket [Unhandled): websocketDidReceiveData \(data)")
            }

        }
    }

    public init(uri: JavaNetURI, connection: Connection) {
        self.uri = uri
        self.connection = connection
    }

    public func send(_ message: String!) {
        print("IOSEndpointWebsocket: \(message!)")
        self.webSocket?.write(string: message)
    }

    public func close() {
        print("IOSEndpointWebsocket: Close")
        self.webSocket?.disconnect()
        self.webSocket = nil
        self.connection.onClose()
    }

    public func forceClose() {
        print("Websocket: Force close")
        self.webSocket?.disconnect(forceTimeout: nil, closeCode: 1)
    }

    public func open() {
        guard let url = URL(string: self.uri.toASCIIString()) else {
            print("IOSEndpointWebsocket Error: Invalid url")
            return
        }

        print("IOSEndpointWebsocket: Open on \(url)")
        self.webSocket = WebSocket(url: url)
        self.webSocket?.connect()
    }
}
