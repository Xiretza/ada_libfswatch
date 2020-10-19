## Ada_libfswatch

This is an Ada binding to the libfswatch library part of the [fswatch](https://github.com/emcrisostomo/fswatch) project.

### Building

This requires the libfswatch library to be installed under `libfswatch/`.  (The best way
to do this is to configure `fswatch` with `--prefix=<path_to_ada_libfswatch>/libfswatch`)

This has been tested with release 1.15.0. 

To build the library, type `make`.
