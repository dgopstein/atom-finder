#include <stdio.h>
#include <math.h>

int main() {
  float V1 = 1.99;
  int V2 = V1; // <true>

  float V3 = 2.87;
  int V4 = trunc(V3); // <false>


  int V5 = -1;
  unsigned int V6 = V5; // <false> maybe someday, but for now we don't know the value of V5


  int V7 = -1;
  unsigned int V8;
  if (V1 >= 0) {
    V8 = V7;
  } else {
    V8 = UINT_MAX + (V7 + 1); // <false>
  }

  char V9 = 54;  // <false>
  char V10 = V9; // <false> - Maybe one day
  V10 = 128;     // <true>

  int V11 = 288;       // <false>
  char V12 = V6 % 256; // <false>

  int V13 = 1, V14 = 2.3; // <true>
}
