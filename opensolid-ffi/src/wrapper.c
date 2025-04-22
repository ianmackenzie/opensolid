#include <stdlib.h>

// Get the definitions of RtsConfig etc.;
// we need the IWYU ("include-what-you-use") pragma
// since RtsConfig etc. are actually defined in RtsAPI.h
// (so clangd things we should #include that instead),
// but including Rts.h is the documented approach
// (see https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/ffi.html#using-own-main)
// and it also brings in some #defines that are needed by RtsAPI.h anyways
#include "Rts.h" // IWYU pragma: keep

// Called from client libraries to initialize the Haskell runtime
void
opensolid_init() {
  // Set up RTS options to enable using multiple OS threads
  int argc = 4;
  char** argv = malloc(argc * sizeof(char*));
  argv[0] = "opensolid-ffi";
  argv[1] = "+RTS";
  argv[2] = "-N";
  argv[3] = "-RTS";

  // Configure the runtime to accept "unsafe" options such as -N
  // (safe because we're not actually forwarding any external/user arguments,
  // just passing the hardcoded ones above)
  RtsConfig rtsConfig = defaultRtsConfig;
  rtsConfig.rts_opts_enabled = RtsOptsAll;
  hs_init_ghc(&argc, &argv, rtsConfig);

  // Clean up
  free(argv);
}

// Called from client libraries to finalize the Haskell runtime
void
opensolid_exit() {
  hs_exit();
}

// Called from client libraries to dynamically allocate memory,
// e.g. for marshalling lists;
// this should ensure that code on the Haskell side
// can safely free objects allocated on the client library side
// since both should be calling the same implementations of malloc and free
void*
opensolid_malloc(size_t bytes) {
  return malloc(bytes);
}

// Called from client libraries to free dynamically allocated memory
void
opensolid_free(void* ptr) {
  free(ptr);
}
