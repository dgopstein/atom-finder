void main() {
   int V1 = 4, V2 = 3;

   int V3 = (V1, V2);
   
   printf("%d\n", V3);
}

void main() {
   int V1 = 1, V2 = 2, V3 = 4, V4 = 10, V5 = 2;
   int V6 = (V1, (V2 , V3), (V4, V5, V3));

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

int cellar_door(int i){
  int a, b, c;
  if(i == 2) return 2,15;
  else if(i == 1) return a=2,b=3,c=6;
  else
    return(1),2,3;
}

void main() {
   int V1 = max_int(1,2);
   int V2 = 1, V3 = (2,3), V4 = (V2++, --V3);
   V2 = V1 += 2, V3 + V2;
   
   printf("%d\n", V1);
}
