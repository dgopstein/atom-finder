int main() {
  int a;
  int b;

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

  // going to add a line
  // in the middle
  a = b;

  // going to remove
  // this line right here
  // in the middle
  a = b;
}
