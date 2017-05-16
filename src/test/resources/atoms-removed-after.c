int main() {
  int a, b, c, d, e;

  a = (b + 1) + 3; // <true>
  b = b + 1;

  a = (c++) + 4;  // <false>

  a = e++ + 5;    // <false>

  a = f++ + 6;    // <false>
}
