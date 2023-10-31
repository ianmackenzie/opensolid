#include <stdlib.h>
#include "HsFFI.h"

__attribute__((constructor))
void call_hs_init()
{
  hs_init(NULL, NULL);
}

__attribute__((destructor))
void call_hs_exit()
{
  hs_exit();
}

void opensolid_free(void *ptr)
{
  free(ptr);
}
