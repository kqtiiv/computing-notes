// fsm.c: Finite State Machines


#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "fsm.h"
#include "token.h"

#define FSM_DEBUG
#undef  FSM_DEBUG


#ifdef FSM_DEBUG
// show_asms( l );
//	Show all existing ASMs on l
//
static void show_asms( FSM_fleet l )
{
	printf( ", there are %d ASMs in fleet: ", l->len );
	for( int p=0; p<l->len; p++ )
	{
		printf( "%d, ", l->actsm[p].number );
	}
	putchar('\n');
}
#endif


static int asm_no = 1;		// next free ASM no..


// FSM_fleet l = fsm_create_fleet( &fsm, &dc );
//	Create a new fleet of ASMs.
//	There can be any number of ASMs running at the same time,
//	each with the same FSM machine, but each having a different fsm no,
//	current state and a piece of private data (created by calling dc).
//	Initially, we create one ASM.
//
FSM_fleet fsm_create_fleet( FSM *fsm, FSM_data_creator dc )
{
	FSM_fleet l = malloc( sizeof(struct FSM_fleet) );
	assert( l != NULL );
	l->fsm	   = fsm;
	l->dc	   = dc;

	// STUB: question 2a, implement missing code to populate
	// the dynarray in *l
	l->cap = 1;
	l->len = 0;
	l->actsm = malloc(sizeof(ASM));

	assert( l->actsm != NULL );
	asm_no     = 1;
	l->actsm[0].number    = asm_no++;
	l->actsm[0].currstate = 0;
	l->actsm[0].data      = l->dc();
	l->len++;
	return l;
}


// fsm_addto_fleet( l, state, data );
//	create a new asm in the given state and append it to the fleet.
//
void fsm_addto_fleet( FSM_fleet l, int state, int data )
{
	if( l->cap == l->len )	// need to grow the dynarray
	{
		// STUB: question 2b, implement this missing code
		l->cap *= 2;
		l->actsm = realloc(l->actsm, l->cap * sizeof(ASM));
	}
	l->actsm[l->len].number    = asm_no++;

	// STUB: question 2b, add this missing code too to set
	// the rest of actsm[len] and inc len
	l->actsm[l->len].currstate = state;
	l->actsm[l->len].data = data;
	l->len++;

	#ifdef FSM_DEBUG
	printf( "debug: cloned new asm %d", asm_no-1 );
	show_asms( l );
	#endif
}


// fsm_removefrom_fleet( l, pos );
//	We've finished with asm [pos], remove it from l and destroy it
//
void fsm_removefrom_fleet( FSM_fleet l, int pos )
{
	assert( pos >= 0 && pos < l->len );
	#ifdef FSM_DEBUG
	printf( "debug: before remove pos %d", pos );
	show_asms( l );
	#endif
	if( pos != l->len-1 )
	{
		memcpy( l->actsm+pos, l->actsm+l->len-1, sizeof(ASM) );
	}
	l->len--;
	#ifdef FSM_DEBUG
	printf( "debug: after remove pos %d", pos );
	show_asms( l );
	#endif
}


//
// succeed( smno, tok, data, state, l );
//	The fsm has hit the success state and succeeded.  Handle it.
//
static void succeed( int smno, int tok, int data, int state, FSM_fleet l )
{
	#ifdef FSM_DEBUG
	printf( "debug: asm %d has SUCCEEDED\n", smno );
	#endif

	FSM_action sact = l->fsm->successaction;
	assert( sact != NULL );
	#ifdef FSM_DEBUG
	printf( "debug: calling success action\n" );
	#endif
	sact( smno, state, tok, data );

	// now mark all ASMs in the fleet as "to be killed"
	// EXCEPT ANY IN STATE 0 (if there are any) if preservestate0..
	ASM *actsm = l->actsm;
	for( int pos=0; pos<l->len; pos++, actsm++ )
	{
		if( actsm->currstate != 0 || ! l->fsm->preservestate0 )
		{
			actsm->currstate = -1;
		}
	}
}


// int matchpos[FSM_MAXARCS];
// int nmatches = 0;
// find_matching_arcs( tok, cs, matchpos, &nmatches );
//	Select one or more matching arc positions from
//	cs->arc[cs->narcs] that match tok.  Fills in
//	matchpos[0..nmatches-1], the indices of each
//	matching arc[], and sets nmatches to the number
//	of matching arcs found.
//
static void find_matching_arcs( int tok, FSM_state *cs, int *mp, int *nm )
{
	FSM_arc *a        = cs->arc;
	int      narcs    = cs->narcs;
	int      nmatches = 0;

	for( int i=0; i<narcs; i++, a++ )
	{
		// STUB: question 2c, implement this missing code
		// to determine whether a->tok matches tok, and if so,
		// append position i to the mp[] array
		if (accepttok( a->tok, tok, nmatches != 0 )) {
			mp[nmatches++] = i;
		}

		// Note: You will need to call a function called
		// accepttok( a->tok, tok, nmatches != 0 ) to
		// determine whether or not tok matches a->tok.
		// accepttok() is in token.c, rather than here in
		// fsm.c, because it's code is specific to the regex
		// application of FSMs.
		//
		// accepttok() has special code to allow ANY_TOK to
		// match any non-NUL character, and DEFAULT_TOK to
		// match only if nothing else has matched..
	}
	*nm = nmatches;
	assert( nmatches > 0 );
}


// bool killall = update_asm( actsm, cs, l, tok, arcpos );
//	update a single asm, upon seeing tok, arcpos is the
//	matching arc number and cs the current FSM state
//
static bool update_asm( ASM *actsm, FSM_state *cs, FSM_fleet l, int tok, int arcpos )
{
	FSM       *fsm          = l->fsm;
	int        successstate = fsm->successstate;
    	FSM_action transact     = fsm->transitionaction;
	int        an	        = actsm->number;
	int        csn          = actsm->currstate;
	int        newstate;			// new state of actsm
	bool       succeeded    = false;	// whether or not we succeeded

	newstate = cs->arc[arcpos].nextstate;

	if( transact != NULL )
	{
		#ifdef FSM_DEBUG
		printf( "debug: calling transition action\n" );
		#endif
		transact( an, csn, tok, actsm->data );
	}

	#ifdef FSM_DEBUG
	printf( "debug: asm %d, new state is %d\n", an, newstate );
	#endif

	assert( newstate <= fsm->nstates );
	actsm->currstate = newstate;
	if( newstate == FAIL )
	{
		#ifdef FSM_DEBUG
		printf( "debug: asm %d FAILS\n", an );
		#endif
	} else
	{
		#ifdef FSM_DEBUG
		printf( "debug: asm %d goes to state %d\n", an, newstate );
		#endif
		if( newstate == successstate )
		{
			succeed( an, tok, actsm->data, newstate, l );
			succeeded = true;
		}
	}
	return succeeded;
}


// fsm_update_fleet( l, tok );
//	Update the fleet l, passing the next token tok to all ASMs.
//	This might cause any number of ASMs to fail (and be removed),
//	or each ASM call an action and then change state, or one
//	ASM to succeed and call a final success action (causing all
//	ASMs to be removed from the fleet so we start again clean).
//	It may also clone a fresh ASM..
//
void fsm_update_fleet( FSM_fleet l, int tok )
{
	#ifdef FSM_DEBUG
	printf( "debug: updating fleet with next token " );
	showtok( tok, stdout );
	printf( "\n" );
	printf( "debug: FSMs are: " );

	for( int pos=0; pos<l->len; pos++ )
	{
		ASM *actsm = &l->actsm[pos];
		printf( "%d (state %d, ", actsm->number, actsm->currstate );
		printf( "data: %d", actsm->data );
		printf( "), " );
	}
	putchar( '\n' );
	fflush( stdout );
	#endif

	bool       killall      = false;
	FSM       *fsm          = l->fsm;
	int        numstates    = fsm->nstates;
	int        successstate = fsm->successstate;
	int        len_at_start = l->len; // musn't include newly
					  // cloned FSMs this pass

	for( int pos=0; pos<len_at_start && !killall; pos++ )
	{
		ASM *actsm = &l->actsm[pos];
		int  csn   = actsm->currstate;
		assert( csn >= 0 );
		assert( csn <= numstates );
		assert( csn != successstate );

		FSM_state *cs = &(fsm->state[csn]);

		// find all arcs matching tok
		int matchpos[FSM_MAXARCS];
		int nmatches  = 0;
		find_matching_arcs( tok, cs, matchpos, &nmatches );

		#ifdef FSM_DEBUG
		printf( "debug: update asm %d (in state %d), "
			"with tok ", actsm->number, csn );
		showtok( tok, stdout );
		putchar( '\n' );

		printf( "debug: %d arcs match token: ", nmatches );
		for( int i=0; i<nmatches; i++ )
		{
			printf( " %d", matchpos[i] );
		}
		printf( "\n" );
		#endif

		assert( nmatches > 0 );

		killall = update_asm( actsm, cs, l, tok, matchpos[0] );

		if( killall ) break;

		// if there are other matches: clone, take no actions
		for( int i=1; i<nmatches && !killall; i++ )
		{
			int      pos      = matchpos[i];
			FSM_arc *a        = &cs->arc[pos];
			int	 newstate = a->nextstate;
			assert( newstate <= numstates );

			#ifdef FSM_DEBUG
			printf( "debug: cloning fresh asm %d into "
				"state %d\n", asm_no, newstate );
			#endif

			int newd = actsm->data;
			fsm_addto_fleet( l, newstate, newd );

			// what if newstate is successstate?
			if( newstate == successstate )
			{
				succeed( asm_no, tok, newd, newstate, l );
				killall = true;
			}
		}
	}

	// remove each failed asm from the fleet - in reverse order

	// STUB: question 2d, write missing code to do this.
	// Hint: use fsm_removefrom_fleet( l, pos ) to remove
	// one failed asm.
	for (int pos=l->len-1; pos>=0; pos--) {
		if (l->actsm[pos].currstate == FAIL) {
			fsm_removefrom_fleet( l, pos );
		}
	} 
}


// fsm_destroy_fleet( l );
//	Destroy the whole fleet of ASMs.
//
void fsm_destroy_fleet( FSM_fleet l )
{
	free( l->actsm );
	free( l );
}
