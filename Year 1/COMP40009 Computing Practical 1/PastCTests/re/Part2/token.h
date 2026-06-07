// token.h: token-related definitions

// a token is an integer capable of storing an ordinary ASCII char,
// or one of several special token values shown here..

// a normal token is an ASCII char, having numeric value 0..255,
// 0 being '\0' i.e. ASCII NUL, the end of the target string

// special tokens are large positive values in order to make
// reordering outgoing arcs easier (by sorting numerically)

#define ANY_TOK         256
#define DEFAULT_TOK     257
#define UNLABELLED_TOK  258

extern void showtok( int tok, FILE * out );
extern char * gettokstr( int tok );
extern bool accepttok( int arctok, int tok, bool alreadymatched );
