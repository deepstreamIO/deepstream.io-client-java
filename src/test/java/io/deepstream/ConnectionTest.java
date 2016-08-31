package io.deepstream;

import com.google.gson.JsonObject;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.net.URISyntaxException;
import java.util.HashMap;

import static junit.framework.TestCase.assertEquals;
import static org.mockito.Mockito.*;

@RunWith( JUnit4.class )
public class ConnectionTest {

    String originalUrl = "originalProtocol://originalHost:originalPort";

    DeepstreamClient deepstreamClientMock;
    EndpointMock endpointMock;
    Connection connection;
    ConnectionStateListener connectionStateListenerMock;
    DeepstreamClient.LoginCallback loginCallback;

    @Before
    public void setUp() throws URISyntaxException {
        this.deepstreamClientMock = mock(DeepstreamClient.class);

        this.endpointMock = new EndpointMock(originalUrl, this.connection);
        this.connection = new Connection(originalUrl, new DeepstreamConfig(), this.deepstreamClientMock, this.endpointMock);
        this.endpointMock.setConnection( this.connection );

        this.connectionStateListenerMock = mock(ConnectionStateListener.class);
        this.loginCallback = mock( DeepstreamClient.LoginCallback.class );

        this.connection.addConnectionChangeListener( this.connectionStateListenerMock);
    }

    @After
    public void tearDown() {

    }

    @Test
    public void initialState() {
        this.endpointMock.sendOpenEvent();
        verifyConnectionState( ConnectionState.AWAITING_CONNECTION );
    }

    @Test
    public void challengeReceivedSendsOriginalUrl() {
        this.endpointMock.sendMessage( MessageBuilder.getMsg(Topic.CONNECTION, Actions.CHALLENGE) );
        verifyConnectionState( ConnectionState.CHALLENGING );
        assertEquals(endpointMock.lastSentMessage, MessageBuilder.getMsg(Topic.CONNECTION, Actions.CHALLENGE_RESPONSE, originalUrl));
    }

    //TODO: Challenge response redirect
    @Test
    public void challengeRedirect() {
    }

    @Test
    public void challengeAck() {
        this.endpointMock.sendMessage( MessageBuilder.getMsg(Topic.CONNECTION, Actions.ACK) );
        verifyConnectionState( ConnectionState.AWAITING_AUTHENTICATION );
    }

    @Test
    public void sendingAuthentication() throws Exception {
        this.challengeAck();
        JsonObject authParams = new JsonObject();
        authParams.addProperty( "name", "Yasser" );
        connection.authenticate( authParams, loginCallback );

        assertEquals(endpointMock.lastSentMessage, MessageBuilder.getMsg( Topic.AUTH, Actions.REQUEST, "{\"name\":\"Yasser\"}" ));
        verifyConnectionState( ConnectionState.AUTHENTICATING );
    }

    @Test
    public void sendingAuthenticationWithNoParams() throws Exception {
        this.challengeAck();
        JsonObject authParams = new JsonObject();
        connection.authenticate( authParams, loginCallback );

        assertEquals(endpointMock.lastSentMessage, MessageBuilder.getMsg( Topic.AUTH, Actions.REQUEST, "{}" ));
        verifyConnectionState( ConnectionState.AUTHENTICATING );
    }

    @Test
    public void gettingValidAuthenticationBack() throws Exception {
        this.sendingAuthentication();

        endpointMock.sendMessage( MessageBuilder.getMsg(Topic.AUTH, Actions.ACK) );

        verifyConnectionState( ConnectionState.OPEN );
        verify( loginCallback, times( 1 ) ).loginSuccess( new HashMap() );
        //verify( loginCallback, times( 0 ) ).loginFailed(); //TODO: Any
    }

    @Test
    public void gettingInValidAuthenticationBack() throws Exception {
        this.sendingAuthentication();

        endpointMock.sendMessage( MessageBuilder.getMsg(Topic.AUTH, Actions.ERROR, Event.NOT_AUTHENTICATED.toString(), "SFail" ));

        verifyConnectionState( ConnectionState.AWAITING_AUTHENTICATION );
        //verify( loginCallback, times( 1 ) ).loginSuccess(); //TODO: Any
        verify( loginCallback, times( 1 ) ).loginFailed(  Event.NOT_AUTHENTICATED, "Fail" );
    }

    @Test
    public void errorsWhenTooManyAuthAttempts() throws  Exception {
        this.sendingAuthentication();

        endpointMock.sendMessage( MessageBuilder.getMsg( Topic.AUTH, Actions.ERROR, Event.TOO_MANY_AUTH_ATTEMPTS.toString(), "STOO_MANY_AUTH_ATTEMPTS" ));
        verify( loginCallback, times( 1 ) ).loginFailed(  Event.TOO_MANY_AUTH_ATTEMPTS, "TOO_MANY_AUTH_ATTEMPTS" );

        JsonObject authParams = new JsonObject();
        authParams.addProperty( "name", "Yasser" );

        connection.authenticate( authParams, loginCallback );
        verify( deepstreamClientMock, times( 1 ) ).onError( Topic.ERROR, Event.IS_CLOSED, "The client\'s connection was closed" );
    }

    private void verifyConnectionState( ConnectionState connectionState) {
        assertEquals( this.connection.getConnectionState(), connectionState);
        verify( this.connectionStateListenerMock, atLeastOnce() ).connectionStateChanged(connectionState);
    }
}