#include <stdio.h>

int main() {
  int a = 0, b = 0;

  a++; // <false>

  b = a++; // <true>

  for (int i = 0; i++ < a; ) ; // <true>

  for (int i = 1; i--; ) ; // <true>

  for (int i = 0; i < a; i++) ; // <false>

  for (int i = 1; i--; a++) ; // <true>

  if (a < b++) ; // <true>

  if (a < b) b++; // <false>

  if (a < b) { a++; b; }; // <false>

  if (a < b) { a++; b; }; // <false>

  putchar(b++); // <true>

}
