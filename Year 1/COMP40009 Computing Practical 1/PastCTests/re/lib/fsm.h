// fsm.h: finite state machine framework..
//	a finite state machine is a collection of numbered states,
//	where each state contains a list of outgoing arcs.
//	a state machine also has a successful end state and a
//	successful final action to perform.

//	an active fsm (an ASM) is a (number, currstate, piece of data)
//	tuple, and we hold a fleet (dynarray) of all ASMs.  i.e. all
//	ASMs use the same fsm rules, but each is in a different state
//	and has it's own piece of private data

//	an arc is: a (token, nextstate) pair.  There may now be >1 arc
//	from a single node with the same token, this is how cloning
//	is handled (allowing any number of clone arcs).
//	if there are >=0 outgoing arcs from one node with the same
//	token, the first is the nextstate of the existing ASM (which
//	is altered), all other arcs cause us to clone a new ASM in
//	that state.

// an action is a pointer to a function that takes:
// - the current ASM number,
// - the current state,
// - the current token (that triggered this action)
// - and the shared data
typedef void (*FSM_action)( int asmno, int state, int tok, int d );

// a single arc
typedef struct
{
	int	   tok;		 	// if we see this token then
	int	   nextstate;	 	// enter this state next
} FSM_arc;


#define FSM_MAXSTATES 256
#define FSM_MAXARCS  128

// and here's the fail state
#define FAIL            -1

// a single state
typedef struct
{
	int	   narcs;		// number of arcs
	FSM_arc    arc[FSM_MAXARCS];    // arc[0..narcs-1]
} FSM_state;

// the whole FSM
typedef struct
{
	int	   nstates;		// number of states
	FSM_state  state[FSM_MAXSTATES];// state[0..nstates-1]
	int	   successstate;	// the success state
	FSM_action successaction;	// the success action
	FSM_action transitionaction;    // the usual transition action
	bool       preservestate0;	// do we need to preserve state 0 ASMs
					// after a success or fail?
} FSM;

// an active FSM
typedef struct
{
	int	   number;		// this ASM's logical number
	int	   currstate;		// the current state
	int        data;		// this ASM's private data
} ASM;

// Create a new piece of shared data for a new ASM
typedef int (*FSM_data_creator)( void );

// a fleet of one or more ASMs (a dynamic array of ASMs)
struct FSM_fleet
{
	int		  len;	  // current number of ASMs
	int		  cap;	  // capacity of dynarray
	ASM 	         *actsm;  // pointer to block of cap ASMs
	FSM		 *fsm;	  // all ASMs will run this fsm
	FSM_data_creator  dc;	  // use this function to create data
};

typedef struct FSM_fleet *FSM_fleet;	// the user-facing ADT

extern FSM_fleet fsm_create_fleet( FSM * fsm, FSM_data_creator dc );
extern void fsm_addto_fleet( FSM_fleet l, int state, int data );
extern void fsm_removefrom_fleet( FSM_fleet l, int pos );
extern void fsm_update_fleet( FSM_fleet l, int tok );
extern void fsm_destroy_fleet( FSM_fleet l );
