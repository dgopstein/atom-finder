#include <stdio.h>
#include <math.h>

int main() {
  float V1 = 1.99;
  int V2 = V1; // <true>

  float V1 = 2.87;
  int V2 = trunc(V1); // <false>


  int V1 = -1;
  unsigned int V2 = V1; // <true>


  int V1 = -1;
  unsigned int V2;
  if (V1 >= 0) {
    V2 = V1;
  } else {
    V2 = UINT_MAX + (V1 + 1); // <false>
  }

  int V1 = 261;
  char V2 = V1; // <true>

  int V1 = 288;
  char V2 = V1 % 256; // <false>
}
