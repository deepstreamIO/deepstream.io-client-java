//
//  JavaUtilProperties+Extension.swift
//  TestApp
//
//  Created by Akram Hussein on 18/12/2016.
//  Copyright (c) 2016 deepstreamHub GmbH. All rights reserved.
//

import Foundation

extension Dictionary where Key: ExpressibleByStringLiteral, Value: Any {
    var toProperties : JavaUtilProperties {
        get {
            let properties = JavaUtilProperties()
            self.forEach { (key, value) in
                properties!.put(withId: key, withId: value)
            }
            return properties!
        }
    }
}
