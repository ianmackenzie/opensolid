#include <stdlib.h>

extern void
hs_init(int* argc, char*** argv);
extern void
hs_exit(void);

void
opensolid_init() {
  int argc = 4;
  char** argv = malloc(argc * sizeof(char*));
  argv[0] = "opensolid-ffi";
  argv[1] = "+RTS";
  argv[2] = "-N";
  argv[3] = "-RTS";
  hs_init(&argc, &argv);
  free(argv);
}

void
opensolid_exit() {
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
