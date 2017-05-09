int main() {
  int a;
  int b;

  a = b++ + 3;

  /* existing multi-line comment
     that hash already been changed
     with other content
  */
  a = b;

  // existing multi-single comment
  // that will eventually be changed
  // with other content
  a = b;

  // Added comment
  a = b++;

  // going to add a line
  // in the middle
  a = b;

  // going to remove
  // this line right here
  // in the middle
  a = b;
}
