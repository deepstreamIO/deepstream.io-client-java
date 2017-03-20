//
//  GSON+Types.swift
//  DeepstreamIO
//
//  Created by Akram Hussein on 18/12/2016.
//  Copyright (c) 2017 deepstreamHub GmbH. All rights reserved.
//

import Foundation

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
