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
  1 + 2 - 3; // <false>
  1 - 2 - 3; // <true>
  1 - 2 % 3; // <true>
  1 % 3 / 4; // <true>
  a ? 1 ? b : 2 : 3;// <true>

  a <= b == c > d;// <false>, underclassify

  !a && b; // <true>
  (!a) && b;

  b = sizeof(a + b);//<false>
  foo(!a);//<false>
  &a + 2;// <true>, overclassify
  *a && b;// <false>
  ++a + 1;// <false>
  Obj->a + b;


  - 2+3; // <true>
  - a+b; // <true>
  - a*b; // <true>
  ! 2 || 3; // <true>
  ! a && b; // <true>
  ! a && ! b; // <true>

   a && !b;
   2 + -3;
   p1 + *p2;
   a + --b;
   a % b++;

  *p1 + i1; // <true>
  *p1 + p2; // <true>, overclassify
  *p1 - i1; // <false>, underclassify
  *pp1 - p2; // <false>, underclassify

  (*p1) + i1;
  *(p1 + i1);

  1 || ! 2 || 1; // <true>
  p1 + *p2 + i1; // <true>

  a == 1 || b == 2; // <false> , underclassify
  ! a ? b : c; // <true>
  sh_round_reg(*ca, mode) + 1; // <false>
  1 || 2 == 3 + 4;// <false> , underclassify
  crtl->calls_eh_return && a; // <false>
  offset >= -252; //<false>
  *str == '\0'; // <false>
  i < 0 ? -i : i; //<true>
  i <= FPR4_REGNUM + 1; //<false>


  /*

    The following cases are from the Operator_precedece_.xlsx file

  */
  1 + 2 * 3; //<true>
  * str ++; //<true>
  - a + b; //<true>
  - a * b; //<true>
  a + b && c; //<true>
  a * b && c; //<true>
  - a && c; //<true>
  a + b || c; //<true>
  a * b || c; //<true>
  - a || c; //<true>
  a || b && c; //<true>
  ! a + b; //<true>
  ! a * b; //<true>
  - ! 0; //<true>
  ! a && b; //<true>
  ! a || b; //<true>
  ! a == b; //<true>
  * p1 + p2; //<true>
  & obj + 4; //<true>
  - a ? b : d; //<true>
  a && b ? c : d; //<true>
  a || b ? c : d; //<true>
  ! a ? c : d; //<true>
  i < 0 ? -i : i; //<true>
  a ? 1 ? b : 2 : 3; //<true>
  a - b + c; //<true>
  a * b - c; //<true>
  - a - c; //<true>
  a - c && b; //<true>
  a - c || b; //<true>
  ! a - c; //<true>
  a - b - c; //<true>
  * a -> b; //<true>

  a < b && b == c; //<false> 
  a < b || b == c; //<false>
  a <= b > c; //<false>, underclassify

  1 + 1 + 1;
  1 * 1 * 1;
  - - a;
  a && b && c;
  a || b || c;
  ! ! a;
  1 < 1 + 1;
  1 < 1 * 3;
  - a > b;
  * a * b;
  - * str;
  * a && b;
  ! * a;
  * str == '\0';
  & * a;
  a ? b + c : d;
  a ? b * c : d;
  a ? * c : d;
  a - b == c;
  * a % b;
  * a - 1;
  a ? b - c : d;
  c + a -> b;
  a . b * c;
  - a . b;
  a . b && c;
  a . b || c;
  ! a . b;
  a . b == c;
  a . b ? c : d;
  a . b - c;
  a . c -> b;

  /*
    Parser thinks it's [:comapre :compare]
    [:compare :compare] is removed from confusing list, might result in underclassify,
    but the overall data should be more accurate.
  */
  simple_value = is_simple<Value_Type>::value;

  a = src2 & ~src0;
  cerr << endl << endl;
  ++*this;
  len1 + len2 - 1;
  dp->setup & 0x3;
  e3 | e2 | e1;
 
  ~str++;//<true>
  --~str;

  a = a << 1 << 2; //<false>, underclassify
  a = a | 2 & 3; //<true>
}
