//
//  JavaUtilProperties+Extension.swift
//  DeepstreamIO
//
//  Created by Akram Hussein on 18/12/2016.
//  Copyright (c) 2017 deepstreamHub GmbH. All rights reserved.
//

import Foundation

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
