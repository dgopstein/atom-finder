int single_statement(){
    int V1 = 1, V2 = V1, V3 = V1;
    V2++;
    ++V2;
    V2--;
    --V2;
    
    printf("%d %d %d\n", V1, V2, V3);
}

int atom_in_assignment(){
    int V1 = 1, V2 = 2, V3 = 3;
    
    V2 += --V1;
    V1 -= --(--V3);
    V3 *= V2++;
    V1 /= ++V2;
    
    printf("%d %d %d\n", V1, V2, V3);
}

int atom_in_argument(){
    int V1 = 2, V2 = 4;
    int V3 = (V1--, ++V2);
    
    printf("%d %d %d\n", ++V1, V2, V3);
}

int loop_statement(){
    int V1 = 0, i = 0;
    for (int i = 1; i <= 5; ++i){
    	++V1;
    }
    for (;++i; ++i){
    	++V1;
    }
    for (;++i;){
    	++V1;
    }
    for (;;);
    for (;;) ++i;
    for (;;++i) ++i;
    
    printf("%d\n", V1);
    
    while(V1++){
    	V1++;
    }
    
     printf("%d\n", V1);
}

int main(){
  single_statement();
  atom_in_assignment();
  atom_in_argument();
  loop_statement();
}
