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

void false_positives() {
  // templates are misparsed as less-than/greater-than
  // https://github.com/freebsd/freebsd/blob/c2b6ea8fa56ce6aba773d820fbf64a4d3efac9f5/sys/net80211/ieee80211_output.c#L2262
  f<T>(a, b) // <false>

  // this doesn't get caught as an atom if the macro is defined
  // #define M1(x) printf x
  // https://github.com/freebsd/freebsd/blob/c2b6ea8fa56ce6aba773d820fbf64a4d3efac9f5/sys/net80211/ieee80211_output.c#L2262
  M1((a, b)); // <false>
}
