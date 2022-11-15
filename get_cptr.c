#include <stdio.h>

#include "ISO_Fortran_binding.h"

void get_cptr(CFI_cdesc_t * x, void ** y)
{
    *y = x->base_addr;
    printf("address = %p\n", *y);
}
