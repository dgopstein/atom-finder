#include <stdio.h>

int main() {
  int a = 0;

  #define M1(x) x+1
  3*M1(2);      // <outer-atom>
  3*(M1(2));
  3*M1(2+5);    // <outer-atom>
  3*M1((2+5));  // <outer-atom>

  #define M2(x) x+1
  3*M2(5<4);     // <outer-atom> <inner-atom>
  3*M2((5<4));   // <outer-atom>
  3%(M2(5<4));   // <inner-atom>
  3%(M2((5<4)));

  #define M3(x,y) x+y+1
  3*M3(5<4, 7&2);     // <outer-atom> <inner-atom>
  3*M3((5<4, 7&2));   // <outer-atom>
  3%(M3(5<4, 7&2));   // <inner-atom>
  3%(M3((5<4, 7&2)));
  3%(M3((5<4), 7&2)); // <inner-atom>
  3%(M3((5<4), (7&2)));

  #define M4 4+1
  3*M4;         // <outer-atom>
  3*(M4);

  // gcc/gcc/testsuite/g++.dg/cpp0x/constexpr-63265.C
  #define LSHIFT 1
  template <int> struct SW1 {int v = 0;};
  SW1<LSHIFT>::v;

  // redis/src/quicklist.c:269
  #define M(x) ((x)->y != 0)
  M(1);

  // redis/deps/lua/src/lgc.c:80
  //#define M(t) t
  //M(k(o));

  #define M(t) f(t)
  M(k(o));
}
