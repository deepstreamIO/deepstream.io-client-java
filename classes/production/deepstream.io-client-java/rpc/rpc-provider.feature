@rpc
Feature: Providing RPC
	Remote Procedure Calls are deepstream's
	concept of request-response communication.
	This requires a client that makes the RPC
	(requestor or receiver) and another client
	that answers it (provider).

	The provider is responsible for notifying the
	server that it is capable of recieving
	requests with a certain name.

	These scenarios covers the provider, which is
	responsible for recieving RPC with a certain
	name, and either accepting to execute them by
	sending back an acknowledgment, or rejecting
	it so that another RPC provider can execute it
	instead.

	If the RPC provider does accept the request,
	it can either respond with a succesful
	callback and the result arguments, or with an
	error and associated error message.

Scenario: RPC Provider
	# The client is connected
	Given the test server is ready
		And the client is initialised
		And the server sends the message C|A+
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

# Providing

	# The client provides a RPC
	When the client provides a RPC called "toUppercase"
	Then the last message the server recieved is P|S|toUppercase+

	# The client gets a subscribe ACK
 	When the server sends the message P|A|S|toUppercase+

# Success

	# The client gets a request that will succeed
 	When the server sends the message P|REQ|toUppercase|<UID>|Sabc+

	# The client responds with an ack and processes message ( 1 )
	Then the last message the server recieved is P|A|toUppercase|<UID>+
 		And the client recieves a request for a RPC called "toUppercase" with data "abc"

	# The client responds with a success
	When the client responds to the RPC "toUppercase" with data "ABC"
 	Then the last message the server recieved is P|RES|toUppercase|<UID>|SABC+

# Error

	# The client gets a request that will error
 	When the server sends the message P|REQ|toUppercase|<UID>|Sabc+

	# The client responds with an ack and processes message ( 2 )
	Then the last message the server recieved is P|A|toUppercase|<UID>+
 		And the client recieves a request for a RPC called "toUppercase" with data "abc"

	# The client responds with an error
	When the client responds to the RPC "toUppercase" with the error "An Error Occured"
 	Then the last message the server recieved is P|E|An Error Occured|toUppercase|<UID>+

# Rejection when supported

	# The client gets another supported request
 	When the server sends the message P|REQ|toUppercase|<UID>|Sabc+

	# The client responds with an ack and processes message ( 3 )
	Then the last message the server recieved is P|A|toUppercase|<UID>+
 		And the client recieves a request for a RPC called "toUppercase" with data "abc"

	# The client responds with an explicit rejection
	When the client rejects the RPC "toUppercase"
 	Then the last message the server recieved is P|REJ|toUppercase|<UID>+

# Rejection when not supported

	# The client gets an unsupported request
	When the server sends the message P|REQ|unSupported|<UID>|Sabc+

	# The client responds with an implicit rejection
	Then the last message the server recieved is P|REJ|unSupported|<UID>+

# Unproviding

	# The client stops providing a RPC
	When the client stops providing a RPC called "toUppercase"
	Then the last message the server recieved is P|US|toUppercase+

	# The client gets an unprovide ACK
 	When the server sends the message P|A|US|toUppercase+