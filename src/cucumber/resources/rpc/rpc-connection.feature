@rpc
Feature: RPC Connectivity
	RPC providers must be resent to the server after connection issues to guarantee it continues to recieve requests correctly.

Scenario: RPC Connectivity

	# The client is connected
	Given the test server is ready
		And the client is initialised
		And the server sends the message C|A+
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

	# The client provides a RPC
	When the client provides a RPC called "toUppercase"
	Then the last message the server recieved is P|S|toUppercase+

	# The client gets an ACK
 	When the server sends the message P|A|S|toUppercase+

	# The client loses it connection to the server
	When the connection to the server is lost
	Given two seconds later
	Then the client throws a "CONNECTION_ERROR" error with message "Can't connect! Deepstream server unreachable on localhost:7777"
		And the clients connection state is "RECONNECTING"

	# The client reconnects to the server
	When the connection to the server is reestablished
		And the server sends the message C|A+
	Then the clients connection state is "AUTHENTICATING"

	# The client successfully reconnects
	Given the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+
	Then the clients connection state is "OPEN"

	# The client resends the RPC provider
	Then the server received the message P|S|toUppercase+