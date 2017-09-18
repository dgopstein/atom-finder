#include <stdio.h>
#include <math.h>

int main(int argc, char **argv) {
  argc = 2; // <true>

  int a[] = {5, 3, 2};

  int i = 0;
  while (i < 3) {
    i++;
  }

  // only catch assignments to function arguments
  a[0] = 2; // <false>
  a[argc] = 3; // <false>
  argv[0] = ''; // <false> - passed by pointer
  argv.x = ''; // <false> - passed by pointer
  argc.x = 2; // <false> - only care about top-level assignments
}
