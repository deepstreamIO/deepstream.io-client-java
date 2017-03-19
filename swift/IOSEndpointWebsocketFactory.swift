//
//  IOSEndpointWebsocketFactory.swift
//  DeepstreamIO
//
//  Created by Akram Hussein on 18/03/2017.
//  Copyright (c) 2017 deepstreamHub GmbH. All rights reserved.
//

import Foundation

public final class IOSEndpointWebsocketFactory : NSObject, EndpointFactory {
    public func createEndpoint(_ uri: JavaNetURI!, connection: Connection!) -> Endpoint! {
        return IOSEndpointWebsocket(uri: uri, connection: connection)
    }
}
