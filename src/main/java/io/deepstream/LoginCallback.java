package io.deepstream;
import java.util.Map;

public interface LoginCallback {

    public void loginSuccess( Map loginData );

    public void loginFailed( String errorEvent );
}
