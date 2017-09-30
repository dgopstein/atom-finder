#include <stdio.h>

int main() {
  int a = 0;

  #define M1(x) x+1
  3*M1(2);      // <outer-atom>
  3*(M1(2));
  3*M1(2+5);    // <outer-atom>
  3*M1((2+5));  // <outer-atom>

  #define M2(x) x*1
  3&M2(5+4);     // <outer-atom> <inner-atom>
  3&M2((5+4));   // <outer-atom>
  3^(M2(5+4));   // <inner-atom>
  3^(M2((5+4)));

  #define M3 4+1
  3*M3;         // <outer-atom>
  3*(M3);

}
