package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

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
    @ObjectiveCName("init:userData:")
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
    @ObjectiveCName("init:errorEvent:data:")
    LoginResult(boolean loggedIn, Event errorEvent, Object data) {
        this.loggedIn = loggedIn;
        this.errorEvent = errorEvent;
        this.data = data;
    }

    /**
     * Whether or not the login occurred successfully
     *
     * @return true if login was successful, false otherwise
     */
    public boolean loggedIn() {
        return this.loggedIn;
    }

    /**
     * Return the data associated with login. If login was successful,
     * this would be the user associated data. Otherwise data explaining
     * the reason why it wasn't.
     * @return A JsonElement containing the data recieved from the server during login
     */
    public Object getData() {
        return this.data;
    }

    /**
     * The error message the server sent to explain why the client couldn't log in.
     * @return an error event
     */
    public Event getErrorEvent() {
        return this.errorEvent;
    }
}
