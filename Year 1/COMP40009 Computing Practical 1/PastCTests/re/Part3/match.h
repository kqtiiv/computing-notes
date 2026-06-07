// match.h

extern void show_mydata( int data, FILE * out );
extern bool match( REFlags flags, FSM * fsm, char * str, int * sp, int * ep );
extern void report_match_str( REFlags flags, FSM * fsm, char * searchstr, char * regex );
extern void report_match_line( REFlags flags, FSM * fsm, char * line, char * filename, bool showlabel, int lineno );
