// Atoms should be counted inside macro-definitions
// but not inside macro-expansions

#define PRECEDENCE 3*2+1 // <true>
#define PRECEDENCES PRECEDENCE || PRECEDENCE || PRECEDENCE || PRECEDENCE || PRECEDENCE || PRECEDENCE // <false>

void main() {
  PRECEDENCES; // <false>
  4*5+6; // <true>
  7 &  PRECEDENCE; // <true>
  (PRECEDENCE); // <false>

  int x;
  if (x) {  // <true> <implicit-predicate>
    PRECEDENCE; // <false>
  }

  if (x == 1) // <true> <omitted-curly-brace>
    PRECEDENCE;

  PRECEDENCE, PRECEDENCE; // <true> <comma-operator>

  PRECEDENCE ? 1 + PRECEDENCE : 2 * PRECEDENCE // <true> <conditional-operator>
}
