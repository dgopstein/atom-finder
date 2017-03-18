#include <stdio.h>

int main() {
}

int V1;

V1 = 4; // <false>

void F1(int V1) {
  V2 = 1;  // <false>
  V1 = 2;  // <true>
  V1 == 3; // <false>
  V1++;    // <true>
  F2(V1);  // <false>
}

V1 = 5; // <false>
