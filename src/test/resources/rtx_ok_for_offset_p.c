#define RTX_OK_FOR_OFFSET_P(MODE, X) \\
(GET_CODE (X) == CONST_INT \\
 && SMALL_INT_RANGE (INTVAL (X), (GET_MODE_SIZE (MODE) - 1) & -4, \\
		     (INTVAL (X) & (GET_MODE_SIZE (MODE) - 1) & 3 \\
		      ? 0 \\
		      : -(-GET_MODE_SIZE (MODE) | -4) >> 1)))

int main() {
  RTX_OK_FOR_OFFSET_P (mode, XEXP (addr, 1));
}
