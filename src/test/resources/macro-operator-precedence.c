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

  #define M(x) { int y = (x); y = f(x); }
  M(1);

  // Can't tell f(x) is a function, I guess?
  //#define M(x) { int y = (x); f(x); }
  //M(1);

  // repeated arg
  #define M(x) x * x
  M(1+2); // <inner-atom>

  #define M(x) (x) * (x)
  M(1+2);

  #define M(x) x*2 + f(x)
  M(1+2); // <inner-atom>

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


  // Occasionally CDT can't write and re-parse correctly
  // e.g. (->> "(size_t)1" parse-frag write-ast (not= "(size_t)1"))
  // Because CDT adds parens around the 1, doesn't realize size_t is a type,
  // and thinks the whole thing is a function call
  //
  // gcc/libffi/src/dlmalloc.c:3464
  //#define page_align(S) (((S) + (mparams.page_size)) & ~(mparams.page_size - SIZE_T_ONE))
  //asize += (page_align((size_t)base) - (size_t)base);
  //
  //#define M(S) S
  //M((size_t)1);"

  // gcc/zlib/contrib/minizip/crypt.h:55
  #define crc32(c, b) ((*(pcrc_32_tab+(((int)(c) ^ (b)) & 0xff))) ^ ((c) >> 8))
  (*(pkeys+2)) = crc32((*(pkeys+2)), keyshift);

  // TODO
  // #define M(c) (int)c
  // M((1));


  // linux/arch/arc/mm/mmap.c:61
  #define M(x) x
  x = M(x);

  // linux/arch/arc/kernel/unaligned.c:150
  #define get32_unaligned_check(val, addr)  unsigned int err = 0, v, a = addr;	\
  get32_unaligned_check(val, state->src1 + state->src2);

  #define M(x) int y = x;
  M(1);
}
