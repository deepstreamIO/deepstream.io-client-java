package io.deepstream.message;

import io.deepstream.ConnectionChangeListener;
import io.deepstream.DeepstreamClient;
import io.deepstream.LoginCallback;
import io.deepstream.constants.Actions;
import io.deepstream.constants.ConnectionState;
import io.deepstream.constants.Event;
import io.deepstream.constants.Topic;
import io.socket.engineio.client.Socket;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.*;
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
    Socket socketMock;
    Connection connection;
    ConnectionChangeListener connectionChangeListenerMock;
    LoginCallback loginCallback;

    @Before
    public void setUp() throws URISyntaxException {
        this.deepstreamClientMock = mock(DeepstreamClient.class);
        this.socketMock = new Socket(originalUrl);
        this.connection = new Connection(originalUrl, new HashMap(), this.deepstreamClientMock, this.socketMock);
        this.connectionChangeListenerMock = mock(ConnectionChangeListener.class);
        this.loginCallback = mock( LoginCallback.class );

        this.connection.addConnectionChangeListener( this.connectionChangeListenerMock );
    }

    @After
    public void tearDown() {

    }

    @Test
    public void initialState() {
        this.socketMock.emit(Socket.EVENT_OPEN);
        verifyConnectionState( ConnectionState.AWAITING_CONNECTION );
    }

    @Test
    public void challengeReceivedSendsOriginalUrl() {
        this.socketMock.emit(Socket.EVENT_MESSAGE, MessageBuilder.getMsg(Topic.CONNECTION, Actions.CHALLENGE));
        verifyConnectionState( ConnectionState.CHALLENGING );
        assertEquals(socketMock.lastSentMessage, MessageBuilder.getMsg(Topic.CONNECTION, Actions.CHALLENGE_RESPONSE, originalUrl));
    }

    //TODO: Challenge response redirect
    @Test
    public void challengeRedirect() {
    }

    @Test
    public void challengeAck() {
        socketMock.emit(Socket.EVENT_MESSAGE, MessageBuilder.getMsg(Topic.CONNECTION, Actions.ACK));
        verifyConnectionState( ConnectionState.AWAITING_AUTHENTICATION );
    }

    @Test
    public void sendingAuthentication() throws JSONException {
        this.challengeAck();
        JSONObject authParams = new JSONObject( "{\"name\":\"Yasser\"}" );
        connection.authenticate( authParams, loginCallback );

        assertEquals(socketMock.lastSentMessage, MessageBuilder.getMsg( Topic.AUTH, Actions.REQUEST, "{\"name\":\"Yasser\"}" ));
        verifyConnectionState( ConnectionState.AUTHENTICATING );
    }

    @Test
    public void gettingValidAuthenticationBack() throws JSONException {
        this.sendingAuthentication();

        socketMock.emit(Socket.EVENT_MESSAGE, MessageBuilder.getMsg(Topic.AUTH, Actions.ACK));

        verifyConnectionState( ConnectionState.OPEN );
        verify( loginCallback, times( 1 ) ).loginSuccess( new HashMap() );
        //verify( loginCallback, times( 0 ) ).loginFailed(); //TODO: Any
    }

    @Test
    public void gettingInValidAuthenticationBack() throws JSONException {
        this.sendingAuthentication();

        socketMock.emit(Socket.EVENT_MESSAGE, MessageBuilder.getMsg(Topic.AUTH, Actions.ERROR, Event.NOT_AUTHENTICATED.toString(), "Fail" ));

        verifyConnectionState( ConnectionState.AWAITING_AUTHENTICATION );
        //verify( loginCallback, times( 1 ) ).loginSuccess(); //TODO: Any
        verify( loginCallback, times( 1 ) ).loginFailed(  Event.NOT_AUTHENTICATED, "Fail" );
    }

    private void verifyConnectionState( ConnectionState connectionState) {
        assertEquals(connection.getConnectionState(), connectionState);
        verify( connectionChangeListenerMock, atLeastOnce() ).connectionStateChanged(connectionState);
    }
}