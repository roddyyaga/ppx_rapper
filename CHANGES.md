# 3.1.0
- Replace deprecated caqti functions (#21, @apeschar)
- `function_out` generate labelled params in the order they're queried (#20, @anmonteiro)
- Add support for `let%rapper` pattern captures (#13, @rizo)
- Added eio support (#32, @anmonteiro)

# 3.0.0
- Split into separate libraries for lwt and async (#4, @bikallem)

# 2.0.0
- Breaking change: moved Caqti connection to be last parameter of queries
- Breaking change: renamed runtime library
- Added `function_out` option
- Added multiple outputs

# 1.2.0
- Added cdate and ctype from Caqti\_type\_calendar (#3, @anmonteiro)

# 1.1.1
- Added support for nested modules for custom types (#2, @anmonteiro)

# 1.1.0
- Bugfixes to make lists work with other parameters

# 1.0.2
- Bugfix to allow use when Base is open

# 1.0.1
- Corrected dependencies

# 1.0.0
- Added support for lists, new types (octets, int32, int64, pdate, ptime, ptime\_span); added runtime library (#1, @anmonteiro)
- Added examples

# 0.9.3
- Added support for floats

# 0.9.2
- Packaging changes

# 0.9.1
- Packaging changes

# 0.9.0
- Initial release
