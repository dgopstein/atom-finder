#include <stdio.h>

int main() {
  int a = 0, b = 0;

  a = 1; // <false>

  b = a = 2; // <true>

  for (int i = 0; i = i + 1 < a; ) ; // <true>

  for (int i = 1; i = i - 1; ) ; // <true>

  for (int i = 0; i < a; i = i + 1) ; // <false>

  for (int i = 1; i = i - 1; a = a + 1) ; // <true>

  if (a < b = b + 1) ; // <true>

  if (a < b) b = b + 1; // <false>

  if (a < b) { a = a + 1; b; }; // <false>

  if (a < b) { a = a + 1; b; }; // <false>

  putchar(b = b + 1); // <true>

}
