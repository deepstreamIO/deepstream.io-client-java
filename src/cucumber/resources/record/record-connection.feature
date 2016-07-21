@records
Feature: Record Connectivity
	Record connectivity is slightly more
	complicated than the other types. We must
	resubscribe listen patterns, however the when
	rerequesting the read action to resubscribe to
	records the result from the server must be
	executed as an update rather than an initial
	read.

	This is because possible merge conflicts may
	occur if versions are not in sync while the
	connection was lost.

Scenario: Record Connectivity
	# The client is connected
	Given the test server is ready
		And the client is initialised
		And the server sends the message C|A+
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

	# The client creates a record
	Given the client creates a record named "connectionRecord"
	Then the last message the server recieved is R|CR|connectionRecord+

	# The server sends a read ACK and read message for connectionRecord
	Given the server sends the message R|A|S|connectionRecord+
		And the server sends the message R|R|connectionRecord|100|{"name":"John", "pets": [{"name":"Ruffles", "type":"dog","age":2}]}+

	# The client listens to recordPrefix
	When the client listens to a record matching "recordPrefix/.*"
	Then the last message the server recieved is R|L|recordPrefix/.*+

	# The server responds with an ACK
	Given the server sends the message R|A|L|recordPrefix/.*+

	# The client loses it connection to the server
	When the connection to the server is lost
	Given two seconds later
	Then the client throws a "CONNECTION_ERROR" error with message "Can't connect! Deepstream server unreachable on localhost:7777"
		And the clients connection state is "RECONNECTING"

	# The client sends an partial update
	When the client sets the record "connectionRecord" "pets[0].name" to "Max"

	# The client reconnects to the server
	When the connection to the server is reestablished
	And the server sends the message C|A+
	Then the clients connection state is "AUTHENTICATING"

	# The client successfully reconnects
	Given the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+
	Then the clients connection state is "OPEN"

	# The client resends the record subscription
	Then the server received the message R|CR|connectionRecord+

	# The client resends the listen record
	Then the server received the message R|L|recordPrefix/.*+

	# The client sends offline changes
	Then the server received the message R|P|connectionRecord|101|pets[0].name|SMax+