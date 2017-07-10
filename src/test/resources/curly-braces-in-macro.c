// Modified from: gcc/gcc/c/c-parser.c

#define POP \
  do { \
    switch (stack[sp].op) { \
      case TRUTH_ORIF_EXPR: 1; \
	break; \
      }	\
  } while (0)

int main() {
  POP;
}
