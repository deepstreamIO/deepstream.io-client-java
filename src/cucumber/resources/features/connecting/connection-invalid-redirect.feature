Feature: Redirecting the client to another deepstream gets rejected
  If the client fails the connection challenge it should then close the connection
  and not allow authentication to occur

  Scenario: The server challenges the client to show which url it wants to connect to
    Given the test server is ready
    And the client is initialised
    When the server sends the message C|CH+
    Then the last message the server recieved is C|CHR|localhost:9696+
    And the clients connection state is "CHALLENGING"

  Scenario: The client closes the connection if the challenge is rejected
    Given the test server is ready
    And the client is initialised
    When the server sends the message C|REJ+
    Then the server has 0 active connections
    And the clients connection state is "CLOSED"

  Scenario: Throws an error if the user attempts to login
    Given the test server is ready
    And the client is initialised
    When the server sends the message C|CH+
    Then the last message the server recieved is C|CHR|localhost:9696+
    When the server sends the message C|REJ+
    When the client logs in with username "XXX" and password "YYY"
    Then the server has received 1 messages
    And the client throws a "IS_CLOSED" error with message "this client's connection was closed"