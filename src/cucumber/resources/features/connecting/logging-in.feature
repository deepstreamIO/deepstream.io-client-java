Feature: Logging In
  As soon as the client is initialised, it creates a connection with
  the server. However the connection is initially in a quarantine
  state until it sends an authentication message. The auth message
  (A|REQ|<JSON authData>) must always be the first message send by
  the client.

  Scenario: The client sends login credentials and authenticates
    Given the test server is ready
    And the client is initialised
    And the server sends the message C|A+
    When the client logs in with username "YYY" and password "XXX"
    Then the last message the server recieved is A|REQ|{"password":"XXX","username":"YYY"}+
    And the clients connection state is "AUTHENTICATING"

  #Scenario: The client receives a login confirmation
    When the server sends the message A|A+
    Then the clients connection state is "OPEN"

  Scenario: The client logs in with an invalid authentication message
    Given the test server is ready
    And the client is initialised
    And the server sends the message C|A+
    When the client logs in with username "XXX" and password "YYY"
    But the server sends the message A|E|INVALID_AUTH_MSG|Sinvalid authentication message+
    Then the last login failed with error "INVALID_AUTH_MSG" and message "invalid authentication message"

  #Scenario: The client's authentication data is rejected
    When the client logs in with username "XXX" and password "ZZZ"
    But the server sends the message A|E|INVALID_AUTH_DATA|Sinvalid authentication data+
    Then the last login failed with error "INVALID_AUTH_DATA" and message "invalid authentication data"

  #Scenario: The client has made too many unsuccessful authentication attempts
    When the client logs in with username "XXX" and password "ZZZ"
    But the server sends the message A|E|TOO_MANY_AUTH_ATTEMPTS|Stoo many authentication attempts+
    Then the last login failed with error "TOO_MANY_AUTH_ATTEMPTS" and message "too many authentication attempts"

  #Scenario: The client can't made further authentication attempts after it received TOO_MANY_AUTH_ATTEMPTS
    When the server resets its message count
    When the client logs in with username "XXX" and password "ZZZ"
    Then the server has received 0 messages
    And the client throws a "IS_CLOSED" error with message "The client's connection was closed"