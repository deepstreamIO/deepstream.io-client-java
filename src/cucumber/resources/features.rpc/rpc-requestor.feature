Feature: Requesting an RPC
  Remote Procedure Calls are deepstream's concept of request-response
  communication. This requires a client that makes the RPC
  (requestor or receiver) and another client that answers it (provider).

  The requestor can make an RPC call and in most cases expected a
  succesful callback.

  However, if something does go wrong, it should also expect an error
  message, which can either be system determined such as
  NO_RPC_PROVIDER or an error thrown by the provider itself,
  like when the incorrect arguments are sent.

  Scenario: The client is connected
    Given the test server is ready
    And the client is initialised
    And the server sends the message C|A+
    And the client logs in with username "XXX" and password "YYY"
    And the server sends the message A|A+

# Success

  #Scenario: The client makes an RPC that will succeed
    When the client requests RPC "toUppercase" with data "abc"
    Then the last message the server recieved is P|REQ|toUppercase|<UID>|Sabc+

  #Scenario: The client gets a request ACK ( 1 )
    When the server sends the message P|A|toUpperCase|<UID>+

  #Scenario: The client receives a succesful response
    When the server sends the message P|RES|toUppercase|<UID>|SABC+
    Then the client recieves a successful RPC callback for "toUppercase" with data "ABC"

# Error

  #Scenario: The client makes an RPC that will fail
    When the client requests RPC "toUppercase" with data "abc"
    Then the last message the server recieved is P|REQ|toUppercase|<UID>|Sabc+

  #Scenario: The client gets a request ACK ( 2 )
    When the server sends the message P|A|toUppercase|<UID>+

  #Scenario: The client receives an error response
    When the server sends the message P|E|RPC Error Message|toUppercase|<UID>+
    Then the client recieves an error RPC callback for "toUppercase" with the message "RPC Error Message"