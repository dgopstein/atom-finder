int main() {
  int a;
  int b;

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

  // going to add a line
  // added new line
  // in the middle
  a = b;

  // going to remove
  // in the middle
  a = b;
}
