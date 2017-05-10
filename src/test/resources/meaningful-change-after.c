int main() {
  int a, b, c, d, e;

  a = b++ + 3;

  /* existing multi-line comment
     that has already been changed
     with other content
  */
  a = b;

  // existing multi-single comment
  // that has already been changed
  // with other content
  a = b;

  // Added comment
  a = b++;

  // No static
  a = c++;

  // going to add a line
  // added new line
  // in the middle
  a = b;

  // going to
  // in the middle
  a = b;
}
