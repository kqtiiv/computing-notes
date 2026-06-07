// makefsm.c:	convert an abstract regex into an fsm.

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

#include "regex.h"
#include "fsm.h"
#include "makefsm.h"
#include "token.h"

#define MAKEFSM_DEBUG
#undef MAKEFSM_DEBUG


//
// arc( state, tok, nextstate );
//	Add an arc from state for tok -> nextstate
//
void arc( FSM_state *state, int tok, int nextstate )
{
	int n = state->narcs++;
	assert( n < FSM_MAXARCS );

	// STUB: question 3a: complete arc()
	state->arc[n].tok = tok;
	state->arc[n].nextstate = nextstate;
}


//
// arc_set( sn, set, nextstate );
//	add arc(sn, member, nextstate) for each member of set
//
void arc_set( FSM_state *sn, char *set, int nextstate )
{
	for( char *p=set; *p != '\0'; p++ )
	{
		arc( sn, *p, nextstate );
	}
}


//
// FSM fsm;
// append_spat( spat, &fsm );
//	Append one or more states to fsm that implement
//	matching for SimplePat spat
//
void append_spat( SimplePat spat, FSM *fsm )
{
	SimpleType t  = spat->t;
	char *set     = spat->s;
	int n         = fsm->nstates-1;
	FSM_state *sn = &fsm->state[n];
	int nplus1    = fsm->nstates++;
	assert( nplus1 < FSM_MAXSTATES );
	fsm->state[nplus1].narcs = 0;

	#ifdef MAKEFSM_DEBUG
	printf( "debug: append_spat: n=%d, nplus1=%d, spat=", n, nplus1 );
	print_SimplePat( stdout, spat );
	putchar( '\n' );
	#endif

	FSM_state *snplus1;
	int        nplus2;

	// STUB: question 3b: all the cases dealing with Stars and
	// Pluses are missing. Implement them following the diagrams
	// shown in the spec - note how the code derives neatly from
	// the diagrams.

	if( t == SimpleType_is_DotPlus		// need extra state?
	||  t == SimpleType_is_Plus )
	{
		snplus1 = &fsm->state[nplus1];
		nplus2  = fsm->nstates++;
		assert( nplus2 < FSM_MAXSTATES );
		fsm->state[nplus2].narcs = 0;
	}
	switch( t )
	{
	case SimpleType_is_Dot:
		arc( sn, ANY_TOK, nplus1 );
		arc( sn, DEFAULT_TOK, FAIL );
		break;
	case SimpleType_is_One:
		arc_set( sn, set, nplus1 );
		arc( sn, DEFAULT_TOK, FAIL );
		break;
	case SimpleType_is_OptDot:
		arc( sn, ANY_TOK, nplus1 );
		arc( sn, UNLABELLED_TOK, nplus1 );
		break;
	case SimpleType_is_Opt:
		arc_set( sn, set, nplus1 );
		arc( sn, UNLABELLED_TOK, nplus1 );
		break;
	case SimpleType_is_DotStar:
		arc( sn, ANY_TOK, n );
		arc( sn, UNLABELLED_TOK, nplus1 );
		break;
	case SimpleType_is_Star:
		arc_set( sn, set, n );
		arc( sn, UNLABELLED_TOK, nplus1 );
		break;
	case SimpleType_is_DotPlus:
		arc( sn, ANY_TOK, nplus1 );
		arc( sn, DEFAULT_TOK, FAIL );
		arc( snplus1, ANY_TOK, nplus1 );
		arc( snplus1, UNLABELLED_TOK, nplus2 );
		break;
	case SimpleType_is_Plus:
		arc_set( sn, set, nplus1 );
		arc( sn, DEFAULT_TOK, FAIL );
		arc_set( snplus1, set, nplus1 );
		arc( snplus1, UNLABELLED_TOK, nplus2 );
		break;
	}
}


//
// cmparcs:
//	sort comparator for two disguised FSM_arc * elements.
//	sort numerically by token (note that the ANY_TOK, DEFAULT_TOK and
//	UNLABELLED_TOK pseudo-tokens have been carefully given values > 255
//	so that this sort comparator does the right thing (puts ANY_TOK,
//	DEFAULT_TOK and UNLABELLED_TOK arcs at the end of any list of arcs).
//
static int cmparcs( const void *a, const void *b )
{
	FSM_arc *at = (FSM_arc *)a;
	FSM_arc *bt = (FSM_arc *)b;
	return at->tok - bt->tok;
}


#ifdef MAKEFSM_DEBUG
//
// show_fsm( re, fsm, when );
//	Show re->fsm debugging
//
static void show_fsm( RE re, FSM *fsm, char *when )
{
	printf( "debug: regex_to_fsm(" );
	char256 assrc;
	stringify_RE( assrc, re );
	printf( "/%s/) %s:\n", assrc, when );

	for( int i=0; i<fsm->nstates; i++ )
	{
		FSM_state *sn = &fsm->state[i];
		printf( "state %d: %d arcs\n", i, sn->narcs );
		for( int j=0; j<sn->narcs; j++ )
		{
			FSM_arc *a = &sn->arc[j];
			int tok = a->tok;
			printf( "  arc %d: token ", j );
			showtok( tok, stdout );
			printf( " -> nextstate " );
			int ns = a->nextstate;

			if( ns != FAIL )
			{
				printf( "%d", ns );
			} else
			{
				printf( "FAIL" );
			}
			putchar( '\n' );
		}
	}
	if( fsm->successstate != -1 )
	{
		printf( "success state is %d\n", fsm->successstate );
	}
}
#endif


// FSM fsm;
// phase1( re, &fsm );
//	Phase 1 conversion of an abstract regex re into a FSM fsm,
//	make a forward pass through the abstract RE's simple patterns,
//	appending the corresponding FSM fragment to the FSM you generate -
//	including temporary UNLABELLED arcs that don't consume the
//	current input token (these handle optionality).
//
void phase1( RE re, FSM *fsm )
{
	fsm->nstates         = 1;
	fsm->state[0].narcs  = 0;
	fsm->successstate    = -1;
	fsm->preservestate0  = false; // do not preserve state 0 active FSMs

	SPList l   = re->l;
	int    nel = l->nel;

	#ifdef MAKEFSM_DEBUG
	printf( "debug: regex_to_fsm(" );
	print_RE( stdout, re );
	printf( "): nel = %d\n", nel );
	#endif

	// append an FSM snippet for each simplepattern
	for( int i=0; i<nel; i++ )
	{
		#ifdef MAKEFSM_DEBUG
		int kind = l->l[i]->t;
		printf( "debug phase 1: i=%d, kind=%d, l[%d]=", i, kind, i );
		print_SimplePat( stdout, l->l[i] );
		printf( "\n" );
		#endif
		append_spat( l->l[i], fsm );
	}
	fsm->nstates--;

	#ifdef MAKEFSM_DEBUG
	show_fsm( re, fsm, "after phase 1" );
	#endif
}


// FSM fsm;
// phase2( re, &fsm );
//	Phase 2 conversion of an abstract regex re into an FSM fsm,
//	handle the endanchor (if selected), or change final unlabelled->default
//
void phase2( RE re, FSM *fsm )
{
	int lastnode = fsm->nstates-1;
	assert( lastnode < FSM_MAXSTATES );
	FSM_state *slast = &fsm->state[lastnode];
	bool altered = false;

	if( re->flags->endanchor )
	{
		int extranode = fsm->nstates;
		assert( extranode < FSM_MAXSTATES );
		FSM_state *sextra = &fsm->state[extranode];
		sextra->narcs = 0;
		fsm->nstates++;

		printf( "debug phase 2: adding extranode=%d\n",
			extranode );

		// STUB: question 3c: this code to deal with an
		// endanchor is missing. complete it (see Figure 3).

		arc( sextra, 0, fsm->nstates );
		arc( sextra, DEFAULT_TOK, FAIL );

		altered = true;
	} else
	{
		// replace final unlabelled arc (if there is one)
		// with default instead, leaving the destination unchanged
		// nb: this could either goto successstate or to FAIL
		int lastarc = slast->narcs-1;
		if( slast->arc[lastarc].tok == UNLABELLED_TOK )
		{
			#ifdef MAKEFSM_DEBUG
			printf( "debug phase 2: changed lastnode %d last "
				"arc %d from unlabelled to default\n",
				lastnode, lastarc );
			#endif
			slast->arc[lastarc].tok = DEFAULT_TOK;
			altered = true;
		}
	}

	#ifdef MAKEFSM_DEBUG
	if( altered ) { show_fsm( re, fsm, "after phase 2" ); }
	#else
	(void)altered;
	#endif

}


// FSM fsm;
// phase3( re, &fsm );
//	Phase 3 conversion of an abstract regex re into an FSM fsm,
//	walk backwards through the FSM, finding node A -->unlabelled--> node B
//	arcs (where B is not the successstate), and copying all outgoing arcs
//	from node B into node A, replacing the unlabelled arc.
//      (we have to make sure there are no duplicate arcs with the same token
//	in a single state, and that the results are in sorted order)
//
void phase3( RE re, FSM *fsm )
{
	bool altered = false;
	for( int cn=fsm->nstates-2; cn>=0; cn-- )
	{
		FSM_state *scn = &fsm->state[cn];

		// find an unlabelled arc outgoing from state cn
		// (if one exists) - if it exists it'll be the last one.
		if( scn->arc[scn->narcs-1].tok != UNLABELLED_TOK ) continue;

		#ifdef MAKEFSM_DEBUG
		printf( "debug phase 3: found unlabelled arc (#%d) out from "
			"state %d\n", scn->narcs-1, cn );
		#endif

		int nextstate  = scn->arc[scn->narcs-1].nextstate;
		FSM_state *sns = &fsm->state[nextstate];

		// remove the final unlabelled arc
		assert( scn->arc[scn->narcs-1].tok == UNLABELLED_TOK );
		scn->narcs--;
		memset( &scn->arc[scn->narcs], '\0', sizeof(FSM_arc) );

		// build a set of tokens in all arcs leaving state cn
		// and look for an ANY_TOK arc leaving state cn
		bool existingtok[UNLABELLED_TOK];
		FSM_arc *any = NULL;
		for( int i=0; i<UNLABELLED_TOK; i++ )
		{
			existingtok[i] = false;
		}
		for( int i=0; i<scn->narcs; i++ )
		{
			FSM_arc     *a   = &scn->arc[i];
			int          tok = a->tok;
			existingtok[tok] = true;
			if( tok == ANY_TOK ) any = a;
		}

		if( any != NULL )
		{
			printf( "debug phase 3: found any arc from state %d: ",
				cn );
			showtok( any->tok, stdout );
			printf( ", nextstate %d\n", any->nextstate );
			fflush(stdout);
			// We no longer use "any".. the code made no sense
			// and broke some unit tests, so I deleted it. may
			// want to delete "any" as well
		}

		// now copy all arcs from the next state
		// into this state (unless they would duplicate
		// an existing token's arc already in this state)
		for( int i=0; i<sns->narcs; i++ )
		{
			FSM_arc *a   = &sns->arc[i];
			int      ns  = a->nextstate;
			int      tok = a->tok;
			if( existingtok[tok] ) continue;
			existingtok[tok] = true;

			#ifdef MAKEFSM_DEBUG
			printf( "debug phase 3: adding arc from state %d "
				"for tok ", cn );
			showtok( tok, stdout );
			printf( " to state %d\n", ns );
			#endif

			arc( scn, tok, ns );
		}

		// finally, sort the arcs (so that the special ones, eg ANY
		// or DEFAULT) go to the end.  without this, the arcs we
		// copied above could lead to an arc token order such as
		// a, b, c, ANY, d, e which is wrong.
		qsort( scn->arc, scn->narcs, sizeof(FSM_arc), &cmparcs );
		altered = true;
	}

	#ifdef MAKEFSM_DEBUG
	if( altered ) { show_fsm( re, fsm, "after phase 3" ); }
	#else
	(void)altered;
	#endif
}


// FSM fsm;
// bool reachable[FSM_MAXSTATES];
// compute_reachable( reachable, &fsm );
//	Compute the reachable[state] mapping for fsm:
//	State 0 is always reachable, and any state that is
//	the nextstate of an arc (other than FAIL) is reachable.
//	All other states are not reachable.
//
static void compute_reachable( bool *reachable, FSM *fsm )
{
	// STUB: question 3d: complete this function.

	for( int i=0; i<FSM_MAXSTATES; i++ )
	{
		reachable[i] = false;
	}
	reachable[0] = true;

	for( int sn=0; sn<fsm->nstates; sn++ )
	{
		FSM_state *sp = &fsm->state[sn];
		int        nt = sp->narcs;

		// STUB: add your code here, roughly speaking
		// "foreach arc, if the arc nextstate is not FAIL
		//  then reachable[that nextstate] = true"
		for (int i=0; i<nt; i++) {
			int ns = sp->arc[i].nextstate;
			if (ns != FAIL) {
				reachable[ns] = true;
			}
		}
	}
}


// FSM fsm;
// bool reachable[FSM_MAXSTATES];
// delete_and_renumber( sno, &fsm, reachable );
//	Ok, delete state sno, shift all later states down one,
//	and renumber all nextstate endpoints > sno. Also
//	shift later elements of reachable[] down one
//
void delete_and_renumber( int sno, FSM *fsm, bool *reachable )
{
	for( int i=sno+1; i<fsm->nstates; i++ )
	{
		memcpy( &fsm->state[i-1], &fsm->state[i], sizeof(FSM_state) );
		reachable[i-1] = reachable[i];
	}
	fsm->nstates--;
	memset( &fsm->state[fsm->nstates], '\0', sizeof(FSM_state) );
	for( int i=0; i<fsm->nstates; i++ )
	{
		FSM_state *p = &fsm->state[i];
		int nt = p->narcs;
		for( int j=0; j<nt; j++ )
		{
			FSM_arc *a = &p->arc[j];
			if( a->nextstate > sno )
			{
				a->nextstate--;
			}
		}
	}
}


// FSM fsm;
// phase4( re, &fsm );
//	Phase 4 conversion of an abstract regex re into an FSM fsm,
//	delete any unreachable states (i.e. that have no arcs going INTO them)
//
void phase4( RE re, FSM *fsm )
{
	// 4a, find all reachable states
	bool reachable[FSM_MAXSTATES];
	compute_reachable( reachable, fsm );
	assert( reachable[fsm->nstates] );

	// 4b. print out unreachable states
	#ifdef MAKEFSM_DEBUG
	for( int i=0; i<fsm->nstates; i++ )
	{
		if( ! reachable[i] )
		{
			printf( "debug: unreachable state %d\n", i );
		}
	}
	#endif

	// 4c. delete any unreachable states
	bool altered = false;
	for( int i=fsm->nstates-1; i>=0; i-- )
	{
		if( ! reachable[i] )
		{
			delete_and_renumber( i, fsm, reachable );
			altered = true;
		}
	}

	// 4d. check that every state has a final DEFAULT arc
	for( int i=0; i<fsm->nstates; i++ )
	{
		FSM_state *p = &fsm->state[i];
		assert( p->narcs > 0 );
		assert( p->arc[p->narcs-1].tok == DEFAULT_TOK );
	}

	#ifdef MAKEFSM_DEBUG
	if( altered ) { show_fsm( re, fsm, "after phase 4" ); }
	#else
	(void)altered;
	#endif
}


// FSM fsm;
// regex_to_fsm( re, &fsm );
//	Convert an abstract regex re into a finite-state-machine fsm,
//	in 4 main phases. nb: can ignore startanchor throughout, because
//	the match infrastructure deals with this now.
//
void regex_to_fsm( RE re, FSM *fsm )
{
	phase1( re, fsm );	// basic fsm build
	phase2( re, fsm );	// handle endanchor
	phase3( re, fsm );	// deal with unlabelled states
	phase4( re, fsm );	// delete any unreachable states

	fsm->successstate = fsm->nstates;

	#ifdef MAKEFSM_DEBUG
	putchar( '\n' );
	show_fsm( re, fsm, "at end" );
	#endif
}
