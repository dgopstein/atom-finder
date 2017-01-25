int main(){
	int a = 1;
	if( a == 1)	a++;
    --a;
    
    if ( a == 1){
    	a++;
        --a;
    }
    else if (a != 1){
    	a++;
        --a;
    }
    else
    	a++;
    a++;
    --a;
    
    for (int i = 0; i < 3; ++i)
    	++i;
        
    while(a < 10)
    	++a;
}