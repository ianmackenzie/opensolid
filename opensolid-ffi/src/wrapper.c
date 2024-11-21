#include <stdlib.h>
#include "HsFFI.h"

void opensolid_init()
{
  hs_init(NULL, NULL);
}

void opensolid_exit()
{
  hs_exit();
}

void* opensolid_malloc(size_t bytes)
{
  return malloc(bytes);
}

void opensolid_free(void *ptr)
{
  free(ptr);
}
