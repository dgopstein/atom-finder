int main(){
  if (0 && 1 || 2) ; // <true>
  if ((0 && 1) || 2 ) ;

  if (0 && 1 || 2 && 3) ; // <true>
  if (0 || 2 || 3);
  if ((0 && 1) || (2 && 3));
  if (0 && (1 || 2) && 3);
  if ((0 && 1 || 2) && 3); // <true>
  if (0 && (1 || 2 && 3)); // <true>

  int a = 3 + 4 * 5 / 8 - 4; // <true>
  int b = 3 + 4 + 5;
  a = 3 + 4;
  a = b += 1;// <false> assignments are generally not confusing by themselves
  a = b /= c *= 1; // <false>

  a = b || (d = c);
  a = 1, 2; // <true>
  a = 1, 2, 3, b = 2, c = 4; // <true>
  b = a = 1;
  a = 1 + 2;
  1 + 2 - 3; // <true> due to non-associative operators
  1 - 2 - 3; // <true>
  1 - 2 % 3; // <true>
  1 % 3 / 4; // <true>
  a ? 1 ? b : 2 : 3;// <true>

  a <= b == c > d;// <true>

  !a && b; // <true>
  (!a) && b;

  b = sizeof(a + b);
}
