#if 0
#else
#endif

#if 1
int main() {
#else
  int notmain() {
#endif  
  
  int V1 = 9
#if 2
    + 7;
#else
  + 6;
#endif

  
  printf("%d\n", V1);

#if 3  
}
#else
}
#endif

#if 4

#else

#endif  
