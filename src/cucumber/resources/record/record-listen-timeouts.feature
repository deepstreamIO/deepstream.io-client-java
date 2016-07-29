@records @timeout
Feature: Record Listen Timeouts
	Whenever a listen subscribe or unsubscribe
	event does not recieve an acknolowdgement from
	the server the client should emit an ack
	timeout error so that the client can attempt
	to retry.

Scenario: Record Listen Timeouts
	# The client is connected
	Given the test server is ready
		And the client is initialised
		And the server sends the message C|A+
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

	# The client listens to recordPrefix
	When the client listens to a record matching "recordPrefix/.*"
	Then the last message the server recieved is R|L|recordPrefix/.*+

	# The server does not respond in time with a listen ACK
	When some time passes
	Then the client throws a "ACK_TIMEOUT" error with message "No ACK message received in time for LISTEN recordPrefix/.*"

	# The client unlistens to recordPrefix
	When the client unlistens to a record matching "recordPrefix/.*"
	Then the last message the server recieved is R|UL|recordPrefix/.*+

	# The server does not respond in time with an unlisten ACK
	#When some time passes
	#Then the client throws a "ACK_TIMEOUT" error with message "No ACK message received in time for recordPrefix/.*"