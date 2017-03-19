//
//  DeepstreamClient+Extensions.swift
//  DeepstreamIO
//
//  Created by Akram Hussein on 17/12/2016.
//  Copyright (c) 2017 deepstreamHub GmbH. All rights reserved.
//

import Foundation

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
