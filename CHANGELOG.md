## [2.0.0] - 2017.02.16

### Features
- Presence functionality. Now able to query for clients and subscribe to presence events. Documentation is [here](https://deepstream.io/docs/client-java/PresenceHandler) and PR [#41](https://github.com/deepstreamIO/deepstream.io-client-java/pull/41).
- Record write acknowledgement. Records are now able to be set with a write acknowledgement synchronously with any errors from storing the record in cache/storage [#52](https://github.com/deepstreamIO/deepstream.io-client-java/pull/52)

### Enhancements

- Support for secure web sockets [#48](https://github.com/deepstreamIO/deepstream.io-client-java/pull/48)
- Improving API to be more inline with js client [#50](https://github.com/deepstreamIO/deepstream.io-client-java/pull/50)

### Bug Fixes

- Fixing redirection logic [#48](https://github.com/deepstreamIO/deepstream.io-client-java/pull/48)
- Allowing client data from login [#51](https://github.com/deepstreamIO/deepstream.io-client-java/pull/51)
- Client now uses null as undefined and can delete values when setting to null [#44](https://github.com/deepstreamIO/deepstream.io-client-java/issues/44)

## [1.0.1] - 2016.11.21

### Bug Fixes
- Fixed travis deploy

## [1.0] - 2016.11.21

### Bug Fixes
- Fixes an issue leading to invalid authentication state changes.

### Enhancements
- Implements updates to message format.

### Breaking Changes
- deepstream.io versions prior to v2.0.0 are no longer compatible with this client.

### Notes
- This client does not currently support the deepstream presence API. This will
  be added in a later release.
