Feature: Connecting a client
  As soon as the client is instantiated
  it establishes a TCP connection to the
  server and awaits login

  Scenario: The test server is idle and awaits connections
    Given the test server is ready
    And the client is initialised
    Then the server has 1 active connections
    And the clients connection state is "AWAITING_CONNECTION"