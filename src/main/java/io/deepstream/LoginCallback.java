package io.deepstream;
import io.deepstream.constants.Event;

import java.util.Map;

public interface LoginCallback {

    public void loginSuccess( Map loginData );

    public void loginFailed( Event errorEvent, Object data );
}
