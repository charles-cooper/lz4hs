#include <stdio.h>
void wrap_free_dctx(/*LZ4F_decompressionContext*/char *dctx) {
  LZ4F_freeDecompressionContext(*dctx);
}
void print_ptr(void **p) {
  printf("%p\n", p);
  printf("%p\n", (void*)*p);
}

