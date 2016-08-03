@events
Feature: Events Connectivity
	Events subscriptions must be resent to the
	server after connection issues to guarantee
	it continues recieving them correctly.

	This applies to both subscribing to events
	and listening to event subscriptions.

Scenario: Client loses connection

	# The client is connected
	Given the test server is ready
		And the client is initialised
		And the server sends the message C|A+
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

	# The client subscribes to an event
	Given the client subscribes to an event named "test1"
	Then the last message the server recieved is E|S|test1+

	# The server sends an ACK message for test1
	Given the server sends the message E|A|S|test1+

	# The client listens to eventPrefix
	When the client listens to events matching "eventPrefix/.*"
	Then the last message the server recieved is E|L|eventPrefix/.*+

	# The server sends an ACK message for eventPrefix
	Given the server sends the message E|A|L|eventPrefix/.*+

	# The client loses its connection to the server
	When the connection to the server is lost
	Given two seconds later
	Then the client throws a "CONNECTION_ERROR" error with message "Can't connect! Deepstream server unreachable on localhost:7777"
		And the clients connection state is "RECONNECTING"

	# The client publishes an event
	When the client publishes an event named "test1" with data "yetAnotherValue"
	Then the server did not recieve any messages

	# The client reconnects to the server
	When the connection to the server is reestablished
	And the server sends the message C|A+
	Then the clients connection state is "AUTHENTICATING"

	# The client successfully reconnects
	Given the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+
	Then the clients connection state is "OPEN"

	# The client resends the event subscription
	Then the server received the message E|S|test1+

	# The client resends the event listen
	Then the server received the message E|L|eventPrefix/.*+

	# The client sends offline events
	Then the server received the message E|EVT|test1|SyetAnotherValue+
