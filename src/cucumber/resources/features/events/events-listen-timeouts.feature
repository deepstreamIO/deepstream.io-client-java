@events @timeout
Feature: Event Listen Timeouts
	Whenever a listen subscribe or unsubscribe
	event does not receive an acknowledgement from
	the server the client should emit an ack
	timeout error so that the client can attempt
	to retry.

	Scenario: Event Listen Timeouts

	# The client is connected
		Given the test server is ready
		And the client is initialised
		And the server sends the message C|A+
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

	# The client listens to eventPrefix
		When the client listens to events matching "eventPrefix/.*"
		Then the last message the server recieved is E|L|eventPrefix/.*+

	# The server does not respond in time with a listen ACK
		When some time passes
		Then the client throws a "ACK_TIMEOUT" error with message "No ACK message received in time for eventPrefix/.*"

	# The client unlistens to eventPrefix
		When the client unlistens to events matching "eventPrefix/.*"
		Then the last message the server recieved is E|UL|eventPrefix/.*+

	 # The server does not respond in time with an unlisten ACK
		When some time passes
		Then the client throws a "ACK_TIMEOUT" error with message "No ACK message received in time for eventPrefix/.*"