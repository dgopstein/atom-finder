void main() {
   int V1 = 4;

   int V2 = V1 == 3 ? 2 : 1;

   printf("%d\n", V2);
}

void main() {
   int V1 = 4;
   int V2 = 3;
   int V3;
  
   if (V1 == 3) {
      V3 = 2;
   } else {
      V3 = 1;
   }

   printf("%d\n", V3);
}

void main() {
   int V1 = 2;
   int V2 = 3;
   int V3 = 1;

   int V4 = (V1 == 2 ? (V3 == 2 ? 1 : 2) : (V2 == 2 ? 3 : 4));

   printf("%d\n", V4);
}

void main() {
   int V1 = 2;
   int V2 = 3;
   int V3 = 1;

   int V4;
   if (V1 == 2) {
      if (V3 == 2) {
         V4 = 1;
      } else {
         V4 = 2;
      }
   } else {
      if (V2 == 2) {
         V4 = 3;
      } else {
         V4 = 4;
      }
   }

   printf("%d\n", V4);
}

void main() {
   int V1 = 2;
   int V2 = 3;
   int V3 = 1;

   int V4 = V1 == 3 ? V2 : V3;

   printf("%d\n", V4);
}

void main() {
   int V1 = 2;
   int V2 = 3;
   int V3 = 1;

   int V4;
   if (V1 == 3){
      V4 = V2;
   }
   else{
      V4 = V3;
   }

   printf("%d\n", V4);
}
