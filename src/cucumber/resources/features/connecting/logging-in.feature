@connectivity
Feature: Logging In
	As soon as the client is initialised, it creates a connection with
	the server. However the connection is initially in a quarantine
	state until it sends an authentication message. The auth message
	(A|REQ|<JSON authData>) must always be the first message send by
	the client.

Scenario: The client logs in with valid authentication credentials

	Given the test server is ready
		And the client is initialised
		And the server sends the message C|A+
	When the client logs in with username "XXX" and password "YYY"
	Then the last message the server recieved is A|REQ|{"username":"XXX","password":"YYY"}+
		And the clients connection state is "AUTHENTICATING"

# The client receives a login confirmation
	When the server sends the message A|A+
	Then the clients connection state is "OPEN"
		And the last login was successful

Scenario: The client logs in with invalid credentials

	Given the client is initialised
	When the client logs in with username "XXX" and password "YYY"
		But the server sends the message A|E|INVALID_AUTH_DATA|Sinvalid authentication data+
	Then the last login failed with error message "invalid authentication data"

# The client's authentication data is rejected
	When the client logs in with username "XXX" and password "ZZZ"
		But the server sends the message A|E|INVALID_AUTH_DATA|Sinvalid authentication data+
	Then the last login failed with error message "invalid authentication data"

# The client has made too many unsuccessful authentication attempts
	When the client logs in with username "XXX" and password "ZZZ"
		But the server sends the message A|E|TOO_MANY_AUTH_ATTEMPTS|Stoo many authentication attempts+
	Then the last login failed with error message "too many authentication attempts"
