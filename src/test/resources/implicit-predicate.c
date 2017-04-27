int main()
{
  int y = 5;

  for (int x = 4; 4 != 4; x++);

  for (int x = 5; 4 - 4; x++); // <true>

  for (int x = 4; 4 == 4; x++);

  for (int x = 4; y = 4; x++); // <true>

  for (int x = 4; 8 * 9 + 4; x++); // <true>

  if (3 < 6)
    if (3 | 6); // <true>

  if (6 & 8) // <true>
    if (2 ^ 9); // <true>

  if (3 > 6);

  if (sizeof(34)); // <true>

  if (y >= 6);

  while (y || 3)
    while (y); // <true>

  while (y <= 34);

  while (y && y);

  return;
}