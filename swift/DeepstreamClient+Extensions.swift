//
//  DeepstreamClient+Extensions.swift
//  TestApp
//
//  Created by Akram Hussein on 17/12/2016.
//  Copyright (c) 2016 deepstreamHub GmbH. All rights reserved.
//

import Foundation

extension DeepstreamClient {
    // Needed as J2ObjC does not translate properties
    // Required explicit getters/setters in Java
    var record : RecordHandler {
        get {
            return self.getRecordHandler()
        }
    }
    
    var event : EventHandler {
        get {
            return self.getEventHandler()
        }
    }
    
    var rpc : RpcHandler {
        get {
            return self.getRpcHandler()
        }
    }
    
    var presence : PresenceHandler {
        get {
            return self.getPresenceHandler()
        }
    }
}
