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

  // static
  a = c++;

  // going to add a line
  // added new line
  // in the middle
  a = b;

  // going to add a line
  // added new atom-adjacent line
  // near an atom
  a = d++;

  // going to
  // in the middle
  a = b;
}
