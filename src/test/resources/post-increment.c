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

  *(a++); // <true>

  (*a)++; // <false>

  while (a++) ; // <true>

  int y = a++; // <true>

  y = (a++, b); // <false>

  y = (a, b++); // <true>

  (a++, b); // <false>

  (a, b++); // <false>

  (a, y = b++); // <true>

  (y = a++, b); // <true>

  (y = a++, y = b++); // <true>

  for (int i = 0; i < a; b++, i++) ; // <false>

  while (a && b--) ; // <true>

  *a++ = b; // <true>

  se->id = server.slowlog_entry_id++; // <true>

  a[b++]; // <true>

  putchar(a->b++); // <true>
}
