#include <stdio.h>

int main() {
  int a = 0;

#define M1(x) x+1    // <def-atom>
  3*M1(2);           // <use-atom>

#define M2(x) (-2*x)  // <def-atom>
  3*M2(2);            // <no-atom>
  3*M2(3+2);          // <use-atom>

#define M3(x, y) x*y // <def-atom>
  M3(2+3, 4+5);      // <use-atom>
  M3((2+3), (4+5));  // <no-atom>

#define M3_2(x, y) (x)*y // <def-atom>
  M3(2+3, 4+5);          // <use-atom>
  M3((2+3), (4+5));      // <no-atom>

#define M4(x, y) (x)*(y) // <def-atom> <param-wrapped>
  !M4(2+3, 4+5);         // <use-atom>
  !(M4(2+3, 4+5));       // <no-atom>

#define M5(x, y) ((x)*(y)) // <no-atom> <param-wrapped>
  !M5(2+3, 4+5);           // <no-atom>

#define M6 printf("a"); /*<def-atom> <param-wrapped>*/\
  printf("b"); \
  printf("c\n");

  M6;        // <use-atom>
  if (a) M6; // <use-atom>

#define M7 do { /*<no-atom> <do-wrapped> <param-wrapped>*/\
    printf("a"); \
    printf("b"); \
    printf("c\n"); \
  } while(0)

  M7;        // <no-atom>
  if (a) M7; // <no-atom>

#define M8 { /*<def-atom> <param-wrapped>*/\
    printf("a"); \
    printf("b"); \
    printf("c\n"); \
  }

  if (a) M8; // <use-atom>
  //else 1;

#define M9(x, y) do { /*<def-atom> <do-wrapped>*/\
      a = x*y; \
  } while (0)

  M9(2+3, 4+5);        // <use-atom>
  if (a) M9(2+3, 4+5); // <use-atom>

#define M10(x, y) do { /*<no-atom> <do-wrapped> <param-wrapped>*/\
      a = (x)*(y); \
  } while (0)

  if (a) M10(2+3, 4+5); // <no-atom>

  // Has additional semicolon at end of definition that breaks if/else's
#define M11(x, y) do { /*<def-atom> <param-wrapped>*/\
      a = (x)*(y); \
  } while (0);

  if (a) M11(2+3, 4+5);    // <use-atom>
  else 1;

  if (a) { M11(2+3, 4+5) } // <no-atom>
  else 1;

  // Even though it's on a single line, multiple statements will brake if-statements
#define M12 1; 2 /*<def-atom> <param-wrapped>*/
  if (a) M12; // <use-atom>
  M12;        // <no-atom>

  // Technically not multiline since it's a single expression
#define M13 (1 + /*<no-atom> <param-wrapped>*/\
  2)
  if (a) M13; // <no-atom>
  M13;        // <no-atom>
}
