@connectivity
Feature: Redirecting a client to another deepstream
	The client should be able to recieve a connection redirect
	to allow it to connect to a seperate deepstream url if needed

Scenario: Redirecting a client to another deepstream

# The test server is idle and awaits connections
	Given the test server is ready
	Given the second test server is ready
		And the second server has 0 active connections

# The client is instantiated and creates a tcp connection
	Given the test server is ready
		And the client is initialised
	Then the server has 1 active connections
		And the clients connection state is "AWAITING_CONNECTION"

# The server challenges the client to show which url it wants to connect to
	When the server sends the message C|CH+
	Then the last message the server recieved is C|CHR|<FIRST_SERVER_URL>+
		And the clients connection state is "CHALLENGING"

# The client attempts to connect to the other server when it recieves a redirect
	When the server sends the message C|RED|<SECOND_SERVER_URL>+
	When some time passes
	Then the server has 0 active connections
		And the client is on the second server
		And the second server has 1 active connections
		And the clients connection state is "AWAITING_CONNECTION"

# The client changes to awaiting authentication when it recieves connection ack
	When the server sends the message C|A+

# The client sends login credentials
	When the client logs in with username "XXX" and password "YYY"
	Then the last message the server recieved is A|REQ|{"password":"YYY","username":"XXX"}+
		And the clients connection state is "AUTHENTICATING"

# The client receives a login confirmation
	When the server sends the message A|A+
	Then the clients connection state is "OPEN"
		And the last login was successful