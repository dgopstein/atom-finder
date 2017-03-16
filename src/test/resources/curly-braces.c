int main(){
    int a = 1;
    if( a == 1)	a++;
    else if (a == 2) --a;
    else --a;

    if( a == 1)	a++;
    else if (a == 2) { --a; }
    else --a;
    
    if ( a == 1){
    	a++;
        --a;
    }
    else if (a != 1){
    	a++;
        --a;
    }
    else {
        a++;
    }
}

int main(){
  int a = 1;
  if (a) ;
  else if (a) ;
  else if (a) ;
  else if (a) ;
  else ;

  if (a) {}

  else if(a) {}
  else {}
}

int main(){
  int a = 1;

  if (a) ;

  if (a) ;
  else if(a) {}
  else ;

}

int main() {
	for (int i = 0; i < 10; ++i) {
		--i;
		++i;
	}
	for (int i = 0; i < 10; ++i) 
		--i;
}

int main() {
	int i = 10;

	while (i > 0) {
		--i;
		++i;
	}
	
	while (i > -1)
		--i;
}

int main() {
	int i = 0;

	do {
		++i;
		--i;
		++i;
	} while (i < 0);

	do	++i;
	while (i < 0);
}

int main() {
	int i = 0;
	switch (i) {
		case 1:{
			printf("%d", i);
			printf("%d\n", ++i);
		}
	}
	switch (i) {
		case 1:	printf("%d\n", i);
		default: printf("We need a bigger boat\n");
	}
        switch (i) 
         default: printf("We definitely need a bigger boat\n");

}
