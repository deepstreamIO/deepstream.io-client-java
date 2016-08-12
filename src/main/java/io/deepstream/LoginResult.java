package io.deepstream;

import com.google.gson.JsonElement;

import java.util.Map;

/**
 * A result of a login, indicating if it was successful or not and
 * associated data.
 */
public class LoginResult {

    final private boolean loggedIn;
    final private Event errorEvent;
    final private Object data;

    /**
     * Called when {@link DeepstreamClient#login(JsonElement)} is successful
     * @param loggedIn true
     * @param userData Optional data that is specific to the user and returned on succesfuly authentication
     */
    LoginResult(boolean loggedIn, Map userData) {
        this.loggedIn = loggedIn;
        this.errorEvent = null;
        this.data = userData;
    }

    /**
     * Called when {@link DeepstreamClient#login(JsonElement)} is unsuccessful
     * @param loggedIn false
     * @param errorEvent error event
     * @param data Contains data associated to the failed login, such as the reason
     */
    LoginResult(boolean loggedIn, Event errorEvent, Object data) {
        this.loggedIn = loggedIn;
        this.errorEvent = errorEvent;
        this.data = data;
    }

    public boolean loggedIn() {
        return this.loggedIn;
    }

    public Object getData() {
        return this.data;
    }

    public Event getErrorEvent() {
        return this.errorEvent;
    }
}
