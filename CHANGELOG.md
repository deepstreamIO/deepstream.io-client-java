## [2.2.0] - 2017.05.29

### Features
- Using the `CU` (create and update) action in `RecordHandler.setData` and `RecordHandler.setDataWithAck`. This is inline with how upserts are now done on the server

### Improvements
- Now generating a jar with all dependencies included during the build

## [2.1.0] - 2017.05.19

### Features
- `RecordHandler.setData` and `RecordHandler.setDataWithAck` allow setting of record data without being subscribed to the record

### Fixes
- fix a memory leak when many frequent record subscriptions and discards. Records and their internals were not being destroyed properly
- handle complex numeric paths, courtesty of [@honorcode](@honorcode)

## [2.0.8] - 2017.04.12

### Fixes
- Calls to `getRecord` are now synchronized. Meaning multiple threads accessing the same record don't result in `MULTIPLE_SUBSCRIPTION` errors
- Added a default global record merge strategy thanks to [@lironsteren](@lironsteren)

## [2.0.7] - 2017.04.05

### Fixes
- Send proper patch and update messages during `setWithAck`

## [2.0.6] - 2017.03.31

### Fixes
- JSONPath: create new properties while setting nested values

## [2.0.5] - 2017.03.07

### Fixes
- Endpoint: don't throw error when ssl == null
- Event: add INVALID_AUTH_DATA enum

## [2.0.1] - 2017.02.17

### Fixes
- Factory: fix incorrect cast of keyset to arraylist (#64)
- List: fix ListEntryChanged listener not called

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
