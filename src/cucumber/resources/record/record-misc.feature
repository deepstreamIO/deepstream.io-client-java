@records
Feature: Record Misc
	This feature covers other possible scenarios that can occur, such
	as the client requesting the same record multiple times.

Scenario: Requesting record multiple times
	The client attempts to request the same record
	multiple times. This should still only trigger
	a single subscribe message to the server and
	the incoming events should be multiplexed on
	the client

	# The client is connected
	Given the test server is ready
		And the client is initialised
		And the server sends the message C|A+
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

	Given the server resets its message count
	When the client creates a record named "doubleRecord"
		And the server sends the message R|A|S|doubleRecord+
		And the server sends the message R|R|doubleRecord|100|{"name":"John", "pets": [{"name":"Ruffles", "type":"dog","age":2}]}+
		And the client creates a record named "doubleRecord"
	Then the last message the server recieved is R|CR|doubleRecord+
		And the server has received 1 messages
