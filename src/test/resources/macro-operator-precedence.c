#include <stdio.h>

int main() {
  int a = 0;

  #define M1(x) x+1
  3*M1(2);      // <outer-atom>
  3*(M1(2));
  3*M1(2+5);    // <outer-atom>
  3*M1((2+5));  // <outer-atom>

  #define M12(x) x+1
  3*M12(2);     // <outer-atom>
  3*(M12(2));

  #define M2 4+1
  3*M2;         // <outer-atom>
  3*(M2);

}
