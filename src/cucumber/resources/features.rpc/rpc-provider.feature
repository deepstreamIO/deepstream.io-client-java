@rpc
Feature: Providing RPC
  Remote Procedure Calls are deepstream's concept of request-response
  communication. This requires a client that makes the RPC
  (requestor or receiver) and another client that answers it (provider).

  The provider is responsible for notifying the server that it is
  capable of recieving requests with a certain name.

  These scenarios covers the provider, which is responsible for
  recieving RPC with a certain name, and either accepting to execute
  them by sending back an acknowledgment, or rejecting it so that another
  RPC provider can execute it instead.

  If the RPC provider does accept the request, it can either respond
  with a succesful callback and the result arguments, or with an
  error and associated error message.

  Scenario: The client is connected
    Given the test server is ready
    And the client is initialised
    And the server sends the message C|A+
    And the client logs in with username "XXX" and password "YYY"
    And the server sends the message A|A+

# Providing

  #Scenario: The client provides a RPC
    When the client provides a RPC called "toUppercase"
    Then the last message the server recieved is P|S|toUppercase+

  #Scenario: The client gets a subscribe ACK
    When the server sends the message P|A|S|toUppercase+

# Success

  #Scenario: The client gets a request that will succeed
    When the server sends the message P|REQ|toUppercase|<UID>|Ssuccess+

  #Scenario: The client responds with an ack and processes message ( 1 )
    Then the server received the message P|A|toUppercase|<UID>+

  #Scenario: The client responds with a success
    Then the last message the server recieved is P|RES|toUppercase|<UID>|SSUCCESS+

# Error

  #Scenario: The client gets a request that will error
    When the server sends the message P|REQ|toUppercase|<UID>|Serror+

  #Scenario: The client responds with an ack and processes message ( 2 )
    Then the server received the message P|A|toUppercase|<UID>+

  #Scenario: The client responds with an error
    Then the last message the server recieved is P|E|An Error Occured|toUppercase|<UID>+

# Rejection when supported

  #Scenario: The client gets another supported request
    When the server sends the message P|REQ|toUppercase|<UID>|Sreject+

  #Scenario: The client responds with an ack and processes message ( 3 )
    Then the server received the message P|A|toUppercase|<UID>+

  #Scenario: The client responds with an explicit rejection
    Then the last message the server recieved is P|REJ|toUppercase|<UID>+

# Unproviding

  #Scenario: The client stops providing a RPC
    When the client stops providing a RPC called "toUppercase"
    Then the last message the server recieved is P|US|toUppercase+

  #Scenario: The client gets an unprovide ACK
    When the server sends the message P|A|US|toUppercase+