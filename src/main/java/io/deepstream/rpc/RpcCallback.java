package io.deepstream.rpc;

import java.util.Map;

public interface RpcCallback {

    void Call( Map data, String RpcResponse );
}
