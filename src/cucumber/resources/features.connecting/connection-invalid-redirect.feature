Feature: Redirecting the client to another deepstream gets rejected
  If the client fails the connection challenge it should then close the connection
  and not allow authentication to occur

  Scenario: The test server is idle and awaits connections
    Given the test server is ready
    Given the second test server is ready
    And the second server has 0 active connections

  #Scenario: The client is instantiated and creates a tcp connection
    Given the test server is ready
    And the client is initialised
    Then the server has 1 active connections
    And the clients connection state is "AWAITING_CONNECTION"

  #Scenario: The server challenges the client to show which url it wants to connect to
    When the server sends the message C|CH+
    Then the last message the server recieved is C|CHR|localhost:9696+
    And the clients connection state is "CHALLENGING"

  #Scenario: The client closes the connection if the challenge is rejected
    When the server sends the message C|REJ+
    And the clients connection state is "CLOSED"

  #Scenario: Throws an error if the user attempts to login
    When the client logs in with username "XXX" and password "YYY"
    Then the server has received 1 messages
    And the client throws a "IS_CLOSED" error with message "The client's connection was closed"