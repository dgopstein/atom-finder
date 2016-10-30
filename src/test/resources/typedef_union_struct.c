// mono/libgc/cord/cordbscs.c
typedef union {
    struct Concatenation {
    	char null;
	char header;
	char depth;	/* concatenation nesting depth. */
	unsigned char left_len;
			/* Length of left child if it is sufficiently	*/
			/* short; 0 otherwise.				*/
#	    define MAX_LEFT_LEN 255
	word len;
	CORD left;	/* length(left) > 0	*/
	CORD right;	/* length(right) > 0	*/
    } concatenation;
    struct Function {
	char null;
	char header;
	char depth;	/* always 0	*/
	char left_len;	/* always 0	*/
	word len;
	CORD_fn fn;
	void * client_data;
    } function;
    struct Generic {
    	char null;
	char header;
	char depth;
	char left_len;
	word len;
    } generic;
    char string[1];
} CordRep;
