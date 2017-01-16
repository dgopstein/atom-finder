int main(){
	int V1 = 1, V2 = V1++, V3 = ++V1;
    V2++;
    ++V2;
    V2--;
    --V2;
    
    printf("%d %d %d\n", V1, V2, V3);
}

int main(){
	int V1 = 1, V2 = 2, V3 = 3;
    
    V2 += --V1;
    V1 -= --(--V3);
    V3 *= V2++;
    V1 /= ++V2;
    
    printf("%d %d %d\n", V1, V2, V3);
}

int main(){
	int V1 = 2, V2 = 4;
    int V3 = (V1--, ++V2);
    
    printf("%d %d %d\n", ++V1, V2, V3);
}

int main(){
	int V1 = 0;
	for (int i = 1; i <= 5; ++i){
    	++V1;
    }
    
    printf("%d\n", V1);
    
    while(V1 <= 10){
    	V1++;
    }
    
     printf("%d\n", V1);
}