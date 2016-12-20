#include <stdio.h>
#include <stdlib.h>

typedef struct _NODATA_T {
  int idx;
  void* prev;
  void* next;
} NODATA_T;

NODATA_T* create_nodata (int n) {
  NODATA_T* pdata = (NODATA_T*)malloc(sizeof(NODATA_T) * n);
  for (int i = 0; i < n; i++) {
    pdata[i].idx = i + 1;
    if (i == (n - 1)) {
      pdata[i].next = pdata;
    } else {
      pdata[i].next = pdata + (i + 1);
    }
    if (i == 0) {
      pdata[i].prev = pdata + (n - 1);
    } else {
      pdata[i].prev = pdata + (i - 1);
    }
  }
  return pdata;
}

NODATA_T* find_target (NODATA_T* pdata, int tot) {
  int n = 1;

#ifdef _PART2
  n = tot / 2;
#endif
  for (int i = 0; i < n; i++) {
    pdata = (NODATA_T*)pdata->next;
  }
  return pdata;
}

void dissoc (NODATA_T* pdata) {
  ((NODATA_T*)pdata->prev)->next = pdata->next;
  ((NODATA_T*)pdata->next)->prev = pdata->prev;
}

int main(int argc, char **argv) {
  int tot = 5;
  if(argc > 1) {
    tot = atoi(argv[1]);
  }
  NODATA_T* pdata = create_nodata(tot);
  NODATA_T* pfirst = pdata;
  for (;tot > 1; tot--) {
    NODATA_T* tg = find_target(pdata, tot);
    dissoc(tg);
    pdata = (NODATA_T*)pdata->next;
  }
  printf ("%d\n",pdata->idx);
  free(pfirst);
  return 0;
}

// part 1
// gcc -Wall -o day19 day19.c -D_PART1; ./day19 3014387

// part 2 ; not good solution , but it works. :(
// gcc -Wall -o day19 day19.c -D_PART2; ./day19 3014387
