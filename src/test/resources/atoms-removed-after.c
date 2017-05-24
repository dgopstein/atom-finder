int main() {
  int a, b, c, d, e, f, g, h, i, j;

  a = a++;         // <false>

  a = (b + 1) + 3; // <true>
  b = b + 1;

  a = (c++) + 4;  // <true> add parens (TODO maybe this should be false, since the atom still happens?)

  a = e++ + 5;    // <false> letter change

  a = f++ + 6;    // <false> no change, follows others
}
