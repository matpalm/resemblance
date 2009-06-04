%-define(UHASH_M,   3995818069). % largish prime < MAX
%-define(UHASH_MAX, 4294967296). % hash max value (2^32)

-define(UHASH_M,   2305843009213693951).  % largish prime (2^61)-1 < MAX
-define(UHASH_MAX, 18446744073709551616). % hash max value (2^64)

-define(SHINGLE_SIZE, 10).
-define(SKETCH_SIZE, 20).
-define(SKETCHES_IN_COMMON_CUTOFF, ?SKETCH_SIZE/2).
