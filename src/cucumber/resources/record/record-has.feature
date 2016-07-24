@records
Feature: Record Has
	Record Has allows you to check whether the record
	exists in server side storage, and is useful to use
	if you only want to load a record and not create it.

Scenario: Record Has

	# The client is connected
	Given the test server is ready
		And the client is initialised
		And the server sends the message C|A+
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

    # The client creates a record, but doesn't get a read
	Given the client creates a record named "hasRecord"
	When the server sends the message R|A|S|hasRecord+

	# The client checks for a non existing record
	Given the client checks if the server has the record "hasRecord"
    Then the client is not told if the record "hasRecord" exists

    When the server sends the message R|H|hasRecord|F+
	Then the client is told the record "hasRecord" doesn't exist

    # The client checks for an existing record
	Given the client checks if the server has the record "hasRecord"
    Then the client is not told if the record "hasRecord" exists

    When the server sends the message R|H|hasRecord|T+
    Then the client is told the record "hasRecord" exists

    # The check gets an error
    Given the client checks if the server has the record "hasRecord"
    When the server sends the message R|E|H|hasRecord|CACHE_RETRIEVAL_TIMEOUT+
    Then the client is told the record "hasRecord" encountered an error checking if record exists

	# The client receives a read
	Given the server sends the message R|R|hasRecord|100|{"name":"John", "pets": [{"name":"Ruffles", "type":"dog","age":2}]}+

 	# The client checks for a record that has been loaded locally
	Given the client checks if the server has the record "hasRecord"
	#Then the the server didn't receive the message R|US|hasRecord+
	Then the client is told the record "hasRecord" exists
