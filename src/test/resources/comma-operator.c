void main() {
   int V1 = 4, V2 = 3;

   int V3 = (V1, V2); // <true>

   printf("%d\n", V3);
}

void main() {
   int V1 = 1, V2 = 2, V3 = 4, V4 = 10, V5 = 2;
   int V6 = (V1, // <true>
             (V2, V3), // <true>
             (V4, V5, V3)); // <true>

   printf("%d\n", V6);
}

void main() {
   int an_array[] = {3,5,3,8,10};
}

int max_int(int a, int b) {
	if (a > b)
    	return a;
   	return b;
}

void main() {
   int V1 = max_int(1,2);
   
   printf("%d\n", V1);
}
