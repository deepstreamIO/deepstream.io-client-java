@records
Feature: Record Snapshot
	Record Snapshot allows you to request for the record data,
	and is useful to use if you only want to load a record data
	without subscribing. If the record doesn't exist an error
	if provided.

Scenario: Record Snapshot

	# The client is connected
	Given the test server is ready
		And the client is initialised
		And the server sends the message C|A+
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

    # The client creates a record, but doesn't get a read
	Given the client creates a record named "snapshotRecord"
	When the server sends the message R|A|S|snapshotRecord+

	# The client requests snapshot for a non existing record
	Given the client requests a snapshot for the record "snapshotRecord"
	Then the client has no response for the snapshot of record "snapshotRecord"

	When the server sends the message R|E|SN|snapshotRecord|RECORD_NOT_FOUND+
	Then the client is told the record "snapshotRecord" encountered an error retrieving snapshot

    # The client requests snapshot for an existing record
	Given the client requests a snapshot for the record "snapshotRecord"
	Then the client has no response for the snapshot of record "snapshotRecord"

    When the server sends the message R|R|snapshotRecord|100|{"name":"John"}+
    Then the client is provided the snapshot for record "snapshotRecord" with data "{"name":"John"}"

  # The client checks for a record that has been loaded locally
	Given the client checks if the server has the record "snapshotRecord"
	#Then the the server didn't receive the message R|US|snapshotRecord+
	Then the client is told the record "snapshotRecord" exists