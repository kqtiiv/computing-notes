extern void arc( FSM_state * state, int tok, int nextstate );
extern void arc_set( FSM_state * sn, char * set, int nextstate );
extern void append_spat( SimplePat spat, FSM * fsm );
extern void phase1( RE re, FSM * fsm );
extern void phase2( RE re, FSM * fsm );
extern void phase3( RE re, FSM * fsm );
extern void delete_and_renumber( int sno, FSM * fsm, bool * reachable );
extern void phase4( RE re, FSM * fsm );
extern void regex_to_fsm( RE re, FSM * fsm );
