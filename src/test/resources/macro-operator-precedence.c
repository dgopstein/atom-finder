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

  // skip every macro with #'s
  #define M(t) f(#t)
  M(k(o));

  // don't capture identifiers inside other identifiers "f[o]f"
  #define M1(o) fof(o) && 1
  M1(uv->v);

  #define M(x) { int y = (x); f(x); }
  M(1);

  // repeated arg
  #define M(x) x * x
  M(1+2); // <inner-atom>

  #define M(x) (x) * (x)
  M(1+2);

  #define M(x) x*2 + f(x)
  M(1+2); // <inner-atomTODO>

  // linux/tools/virtio/linux/err.h:17
  #define IS_ERR_VALUE(x) unlikely((x) >= (unsigned long)-MAX_ERRNO)
	IS_ERR_VALUE((unsigned long)ptr);

  // linux/lib/zstd/huf_compress.c:96
  #define M(x) (int)(x)
  M(2);

  #define M(x) (int)x
  M(2);

  #define M(x) (x)
  M((int)2);

  #define M(x) x
  M((int)2);
}
