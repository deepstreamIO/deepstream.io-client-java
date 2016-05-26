Feature: Redirecting a client to another deepstream
  The client should be able to recieve a connection redirect
  to allow it to connect to a seperate deepstream url if needed

  Scenario: The client attempts to connect to the other server when it recieves a redirect
    Given the test server is ready
    And the client is initialised
    When the server sends the message C|RED|http://localhost:8898+
    And the server has 0 active connections
    When some time passes
    And the second server has 1 active connections
    And the clients connection state is "AWAITING_CONNECTION"

  #Scenario: The client changes to awaiting authentication when it recieves connection ack
    #When the server sends the message C|A+

  #Scenario: The client sends login credentials
    #When the client logs in with username "XXX" and password "YYY"
    #Then the last message the server recieved is A|REQ|{"username":"XXX","password":"YYY"}+
    #And the clients connection state is "AUTHENTICATING"

  #Scenario: The client receives a login confirmation
    #When the server sends the message A|A+
    #Then the clients connection state is "OPEN"
    #And the last login was successful