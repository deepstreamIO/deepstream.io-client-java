@records
Feature: Record Conflicts
	Record conflicts occur most often when the
	client disconnects, does offline work and then
	reconnects resulting in a merge issue with
	other potential edits, but can also occur in
	rare conditions as part of a race condition.

	If a conflict does occur, the client should
	recieve a VERSION_EXISTS error.

Background:
	Given the test server is ready
	And the client is initialised
	And the server sends the message C|A+
	And the client logs in with username "XXX" and password "YYY"
	And the server sends the message A|A+
	And the client creates a record named "mergeRecord"
	And the server sends the message R|A|S|mergeRecord+
	And the server sends the message R|R|mergeRecord|100|{"key":"value1"}+
	And the client sets the record "mergeRecord" "key" to "anotherValue"


Scenario: Remote Merging Strategy On Read
	When the client selects "REMOTE_WINS" merge strategy for record "mergeRecord"
	And the server sends the message R|R|mergeRecord|104|{"key":"value3"}+
	Then the last message the server recieved is R|U|mergeRecord|105|{"key":"value3"}+

Scenario: Local Merge Strategy On Read
	When the client selects "LOCAL_WINS" merge strategy for record "mergeRecord"
	And the server sends the message R|R|mergeRecord|104|{"key":"value3"}+
	Then the last message the server recieved is R|U|mergeRecord|105|{"key":"anotherValue"}+

Scenario: Custom Merge Strategy On Read
	When the client selects "CUSTOM" merge strategy for record "mergeRecord"
	And the server sends the message R|R|mergeRecord|104|{"key":"value3"}+
	Then the last message the server recieved is R|U|mergeRecord|105|{"key":"customValue"}+

Scenario: Remote Merging Strategy On Update
	When the client selects "REMOTE_WINS" merge strategy for record "mergeRecord"
	And the server sends the message R|U|mergeRecord|104|{"key":"value3"}+
	Then the last message the server recieved is R|U|mergeRecord|105|{"key":"value3"}+

Scenario: Local Merge Strategy On Update
	When the client selects "LOCAL_WINS" merge strategy for record "mergeRecord"
	And the server sends the message R|U|mergeRecord|104|{"key":"value3"}+
	Then the last message the server recieved is R|U|mergeRecord|105|{"key":"anotherValue"}+

Scenario: Custom Merge Strategy On Update
	When the client selects "CUSTOM" merge strategy for record "mergeRecord"
	And the server sends the message R|U|mergeRecord|104|{"key":"value3"}+
	Then the last message the server recieved is R|U|mergeRecord|105|{"key":"customValue"}+

Scenario: Remote Merging Strategy On Patch
	When the client selects "REMOTE_WINS" merge strategy for record "mergeRecord"
	And the server sends the message R|P|mergeRecord|104|key|Svalue3+
	Then the last message the server recieved is R|SN|mergeRecord+
	And the server sends the message R|R|mergeRecord|104|{"key":"value3"}+
	Then the last message the server recieved is R|U|mergeRecord|105|{"key":"value3"}+

Scenario: Local Merging Strategy On Patch
	When the client selects "LOCAL_WINS" merge strategy for record "mergeRecord"
	And the server sends the message R|P|mergeRecord|104|key|Svalue3+
	Then the last message the server recieved is R|SN|mergeRecord+
	And the server sends the message R|R|mergeRecord|104|{"key":"value3"}+
	Then the last message the server recieved is R|U|mergeRecord|105|{"key":"anotherValue"}+

Scenario: Remote Merging Strategy On Patch
	When the client selects "CUSTOM" merge strategy for record "mergeRecord"
	And the server sends the message R|P|mergeRecord|104|key|Svalue3+
	Then the last message the server recieved is R|SN|mergeRecord+
	And the server sends the message R|R|mergeRecord|104|{"key":"value3"}+
	Then the last message the server recieved is R|U|mergeRecord|105|{"key":"customValue"}+

