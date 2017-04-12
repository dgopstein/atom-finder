int main(){
  if (0 && 1 || 2) ; // <true>
  if ((0 && 1) || 2 ) ;

  if (0 && 1 || 2 && 3) ; // <true>
  if ((0 && 1) || (2 && 3));
  if (0 && (1 || 2) && 3);
  if ((0 && 1 || 2) && 3); // <true>
  if (0 && (1 || 2 && 3)); // <true>

  int a = 3 + 4 * 5 / 8 - 4; // <true>
  int b = 3 + 4 + 5;
  a = 3 + 4;

  a = b || a = c; // <true>

  a = 1, 2; // <true>

  b = a = 1; // <false>

  1 + 2 - 3; // <false>

  a ? 1 ? b : 2 : 3; // <true> - Although I could be persuaded to ignore this example

}
