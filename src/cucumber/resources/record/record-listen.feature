@records
Feature: Record Listen
	Listening to Record usage allows the client to
	reduce the amount of records it needs to
	update. If a client requests a record that
	matches your pattern or when all the clients
	discard it you'll be told so you can only need
	to update records that you know are being
	listened used.

	This helps reduce the amount of updates you
	have in the system at any time. Please note,
	when using records to update values within
	the deepstream database which is also queried
	you will need to ensure those records are
	requested for the query to work with the
	latest data, or avoid using listen for those
	records.

Scenario: Record Listen

	# The client is connected
	Given the test server is ready
		And the client is initialised
		And the server sends the message C|A+
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

	# The client listens to recordPrefix
	When the client listens to a record matching "recordPrefix/.*"
	Then the last message the server recieved is R|L|recordPrefix/.*+

	# The server responds with a listen ACK
	Given the server sends the message R|A|L|recordPrefix/.*+

	# The client gets notified of new matching subscriptions
	Given the server sends the message R|SP|recordPrefix/.*|recordPrefix/foundAMatch+
	Then the client will be notified of new record match "recordPrefix/foundAMatch"

	# The client gets notified for removed subscriptions
	Given the server sends the message R|SR|recordPrefix/.*|recordPrefix/foundAMatch+
	Then the client will be notified of record match removal "recordPrefix/foundAMatch"

	# The client unlistens to recordPrefix
	When the client unlistens to a record matching "recordPrefix/.*"
	Then the last message the server recieved is R|UL|recordPrefix/.*+

	# The server responds with an unlisten ACK
	#TODO
	#Given the server sends the message R|A|UL|recordPrefix/.*+

	# Following server updates will throw an error
	Given the server sends the message R|SP|recordPrefix/.*|recordPrefix/foundAMatch+
	#TODO: This error message isn't great
	And the client throws a "UNSOLICITED_MESSAGE" error with message "recordPrefix/.*"