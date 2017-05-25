int main() {
  int a, b, c, d, e, f, g, h, i, j;

  a = a++;      // <false>

  a = b++ + 3;  // <true>

  a = c++ + 4;  // <true> add parens (TODO maybe this should be false, since the atom still happens, just nested more?)

  a = d++ + 5;  // <false> letter change

  a = f++ + 6;  // <false> no change, follows others
}
