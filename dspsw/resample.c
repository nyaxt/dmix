#include <stdio.h>

typedef struct polyphase_filter
{
    const int* rhtaps;
    unsigned nrhtaps;
    int ups;
    int dec;
    int half_depth;
} polyphase_filter_t;

int muladdshr(const int* x, const int* c, unsigned len, int dir)
{
  int sum = 0;
  if (dir > 0) {
    for (int i = 0; i < len; ++i)
      sum += (((long)*x++ * (long)*c++) >> 23) & 0xFFFFFFFFUL;
  } else {
    for (int i = 0; i < len; ++i)
      sum += (((long)*x++ * (long)*c--) >> 23) & 0xFFFFFFFFUL;
  }
  return sum;
}

int polyphase_resample(const polyphase_filter_t* filter, int* y, const int* x, unsigned xlen)
{
  const int* xp = x;
  const int* const xe = x + xlen - filter->half_depth*2;
  int* yp = y;

  int firidx = 0;
  while (xp != xe) {
    int offset = firidx * filter->half_depth;
    int left = muladdshr(xp, &filter->rhtaps[filter->nrhtaps-1 - offset], filter->half_depth, -1);
    int right = muladdshr(xp + filter->half_depth, &filter->rhtaps[offset], filter->half_depth, 1);

    *yp++ = left + right;

    firidx += filter->dec;
    if (firidx >= filter->ups) {
      firidx -= filter->ups;
      xp++;
    }
  }

  return yp - y;
}

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "filter.h"

size_t read_samples(int** x, const char* filepath)
{
  FILE* f = fopen(filepath, "r");
  uint32_t xlen = 0;
  size_t nread = fread(&xlen, sizeof(xlen), 1, f);
  assert(nread == 1);
  printf("num input samples: %u\n", xlen);
  int *xp = (int*)malloc(sizeof(int) * xlen);
  assert(xp);
  *x = xp;
  for (size_t i = 0; i < xlen; ++i) {
      int32_t i32;
      nread = fread(&i32, sizeof(int32_t), 1, f);
      assert(nread == 1);
      *xp++ = i32;
  }
  fclose(f);

  return xlen;
}

void write_samples(const int* y, size_t ylen, const char* filepath)
{
  FILE* f = fopen(filepath, "w");
  uint32_t ylen32 = ylen;
  size_t nwrite = fwrite(&ylen, sizeof(ylen), 1, f);
  assert(nwrite == 1);
  printf("num output samples: %zu\n", ylen);
  for (size_t i = 0; i < ylen; ++i) {
      int32_t i32 = *y++;
      nwrite = fwrite(&i32, sizeof(int32_t), 1, f);
      assert(nwrite == 1);
  }
  fclose(f);
}

int main(int argc, char* argv[])
{
  polyphase_filter_t* filter = &filter_441_48;

  assert(argc == 3);
  int *x;
  size_t xlen = read_samples(&x, argv[1]);

  uint32_t ylen = xlen * filter->ups / filter->dec;
  int* y = (int*)malloc(sizeof(int) * ylen);
  ylen = polyphase_resample(filter, y, x, xlen);

  write_samples(y, ylen, argv[2]);
}
