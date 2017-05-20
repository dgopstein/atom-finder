int main() {
  int a, b, c, d, e, f, g, h, i, z = 0;

  a = z++;

  // <false>
  b = z + 1;

  // <true>
  c = z++;

  /* <true> */
  d = z++;

  /* <true>







  */

  e = z++;

  f = z + 1; /* should be false <false>, but for laziness sake I'll call it <true> */
  g = z++; /* <true> */
  h = z + 1; // should be false <false>, but for laziness sake I'll call it <true>
  i = z++; // <true>
}
