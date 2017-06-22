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

  b = sizeof(a + b);//<false>
  foo(!a);//<false>
  &a + 2;// <true>, should be false
  *a && b;// <true>, should be false
  ++a + 1;// <true>, should be false
  Obj->a + b;


  - 2+3; // <true>
  - a+b; // <true>
  - a*b; // <true>
  ! 2 || 3; // <true>
  ! a && b; // <true>
  a && !b;
  ! a && ! b; // <true>
  

  *p1 + i1; // <true>
  *p1 + p2; // <true>, should be false
  *p1 - i1; // <true>
  *pp1 - p2; // <true>, should be false

  (*p1) + i1;
  *(p1 + i1);

  1 || ! 2 || 1; // <true>
  p1 + *p2 + i1; // <true>

  a == 1 || b == 2; // <true>, but shows up a lot and not really confusing
  ! a ? b : c; // <true>
  sh_round_reg(*ca, mode) + 1; // <true>, why? 
  1 || 2 == 3 + 4;// <true>
  crtl->calls_eh_return && a; // <true>, why?
  offset >= -252; //<true>, should be false
  *str == '\0'; //<true>, should be false
  i < 0 ? -i : i; //<true>
  i <= FPR4_REGNUM + 1; //<true>, should this be false?
}
