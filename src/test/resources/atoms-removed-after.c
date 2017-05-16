int main() {
  int a, b, c, d, e, f, g, h, i, j;

  a = a++;         // <false>

  a = (b + 1) + 3; // <true>
  b = b + 1;

  a = (c++) + 4;  // <false> add parens

  a = e++ + 5;    // <false> letter change

  a = f++ + 6;    // <false> no change, follows others
}
