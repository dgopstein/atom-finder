#include <stdio.h>

int main() {
  int a = 0, b = 0, c = 0;

  b = c; // <false>

  a = b = c; // <true>

  a = b = 0; // <true>

  b = c = a = 0; // <true>

  for (int i; (i = 0) < a; ) ; // <true>

  for (int i = 1; i = 0; ) ; // <true>

  for (int i = 0; i < a; i = 0) ; // <false>

  for (int i = 1; i = 0; a++) ; // <true>

  if (a < (b = 0)) ; // <true>

  if (a < b) b = 0; // <false>

  if (a < b) { b = 0; b; }; // <false>

  if (a < b) { a = 0; b; }; // <false>

  putchar(b = 0); // <true>

  *(b = 0); // <true>

  (*b) = 0; // <false>

  while (a = 0) ; // <true>

  int y = (b = 0); // <true>

  y = (a = 0, b); // <false>

  y = (a, b = 0); // <true>

  (a = 0, b); // <false>

  (a, b = 0); // <false>

  (a, y = b = 0); // <true>

  (y = a = 0, b); // <true>

  (y = a = 0, y = b = 0); // <true>

  for (int i = 0; i < a; b = 0, i = 0) ; // <false>

  while (a && (b = 0)) ; // <true>

  se->id = server.slowlog_entry_id = 0; // <true>

  a[b = 0]; // <true>

  putchar(a->b = 0); // <true>
}
