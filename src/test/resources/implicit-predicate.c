int main()
{
  int y = 5;

  if (y); // <true>

  (y == 5) ? y = 5 : y == 7;

  for (int x = 4; 4 != 4; x++);

  for (int x = 5; 4 - 4; x++); // <true>

  for (int x = 4; 4 == 4; x++);

  (y = 4) ? y : y++; // <true>

  for (int x = 4; 8 * 9 + 4; x++); // <true>

  if (3 < 6)
    if (3 | 6); // <true>

  if (6 & 8) // <true>
    if (2 ^ 9); // <true>

  if (3 > 6);

  ((y != 5)) ? y : y;

  if (sizeof(34)); // <true>

  if ((y >= 6));

  while (y || 3)
    while (y); // <true>

  while (y <= 34);

  (((y = 2))) ? 4 : 5; // <true>

  while (!(y && y));

  if (!(5 == y));

  !y; // <false>

  !y ? 1 : 2; // <false>

  if (unknown_type(1)); // <false> can't resolve type

  return;
}
