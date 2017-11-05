// Atoms should be counted inside macro-definitions
// but not inside macro-expansions

#define PRECEDENCE 3*2+1 // <true>
#define PRECEDENCES PRECEDENCE || PRECEDENCE || PRECEDENCE || PRECEDENCE || PRECEDENCE || PRECEDENCE // <false>

void main() {
  PRECEDENCES; // <false>
  4*5+6; // <true>
  7&PRECEDENCE;
}
