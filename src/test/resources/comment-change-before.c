int main() {
  int a, b, c, d, e;

  b = a + 1;
  b = a + 2;

  /* existing multi-line comment
     that will eventually be changed
     with other content
  */
  a = b;

  // existing multi-single comment
  // that will eventually be changed
  // with other content
  a = b;

  a = b++;

  // Static
  a = c++;

  // going to add a line
  // in the middle
  a = b;

  // going to add a line
  // near an atom
  a = d++;

  // going to
  // remove this line
  // in the middle
  a = b;
}
