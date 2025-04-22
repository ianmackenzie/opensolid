#include <stdlib.h>

// Path to Rts.h will be set by GHC when compiling
#include "Rts.h"

void
opensolid_init() {
  // Defined in Rts.h
  RtsConfig rtsConfig = defaultRtsConfig;
  rtsConfig.rts_opts_enabled = RtsOptsAll;

  // Set up RTS options to enable using multiple OS threads
  int argc = 4;
  char** argv = malloc(argc * sizeof(char*));
  argv[0] = "opensolid-ffi";
  argv[1] = "+RTS";
  argv[2] = "-N";
  argv[3] = "-RTS";

  // Defined in Rts.h
  hs_init_ghc(&argc, &argv, rtsConfig);

  free(argv);
}

void
opensolid_exit() {
  // Defined in Rts.h
  hs_exit();
}

void*
opensolid_malloc(size_t bytes) {
  return malloc(bytes);
}

void
opensolid_free(void* ptr) {
  free(ptr);
}
