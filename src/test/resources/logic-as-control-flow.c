void main() {
   int V1 = 1;
   int V2 = 5;

   if (++V1 || ++V2) { // <true>
      V1 = V1 * 2;
      V2 = V2 * 2;
   }

   printf(""%d %d\n"", V1, V2);
}
void main() {
   int V1 = 2;
   int V2 = 4;

   if (++V1) {
        V1 = V1 * 2;
        V2 = V2 * 2;
   } else if (++V2) {
        V1 = V1 * 2;
        V2 = V2 * 2;
   }

   printf(""%d %d\n"", V1, V2);
}
void main() {
   int V1 = 1;
   int V2 = 5;

   V1 == V2 && ++V1 || ++V2; // <true>

   printf(""%d %d\n"", V1, V2);
}
void main() {
   int V1 = 2;
   int V2 = 6;

   if (V1 == V2) {
      ++V1;
   } else {
      ++V2;
   }

   printf(""%d %d\n"", V1, V2);
}
void main() {
   int V1 = 3;
   int V2 = 5;
   int V3 = 0;

   while (V1 != V2 && ++V1) { // <true>
      V3++;
   }

   printf(""%d %d %d\n"", V1, V2, V3);
}
void main() {
   int V1 = 1;
   int V2 = 11;
   int V3 = 0;

   while (V1 != V2) {
      ++V1;
      if (!V1) break;

      V3++;
   }

   printf(""%d %d %d\n"", V1, V2, V3);
}

int f() {
}

void random() {
  int x = 1, y = 2, z = 3;

  x && f(); // <false>
  x && x = 2; // true
}
