package io.deepstream;

import com.google.gson.JsonElement;
import io.deepstream.constants.Event;

import java.util.Map;

/**
 * A callback that notifies the user if the login process was completed successfully or not, and contains optional data
 * received from the server associated to the user
 */
public interface LoginCallback {

    /**
     * Called when {@link DeepstreamClient#login(JsonElement)} is successful
     * @param userData Optional data that is specific to the user and returned on succesfuly authentication
     */
   void loginSuccess( Map userData );

    /**
     * Called when {@link DeepstreamClient#login(JsonElement)} is unsuccessful
     * @param errorEvent error event
     * @param data Contains data associated to the failed login, such as the reason
     */
   void loginFailed(Event errorEvent, Object data );
}
