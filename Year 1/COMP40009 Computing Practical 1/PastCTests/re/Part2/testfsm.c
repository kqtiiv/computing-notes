// testfsm.c: unit test program for fsm.c

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "testutils.h"
#include "fsm.h"
#include "token.h"

typedef char char256[256];
typedef char char128[128];

static bool succeeded = false;
static int  winner    = -1;


static void mysucceed( int smno, int state, int nextch, int d )
{
	succeeded = true;
	winner    = smno;
	//printf( "success: ASM %d, set winner=%d\n", smno, winner );
}


static int new_mydata( void )
{
	return -1;
}


static FSM myfsm = 	// FSM representation of simple regex a*b+[cd]
{
	.nstates          = 2,
	.successstate     = 2,
	.preservestate0   = false, // do not preserve state 0 ASMs
	.successaction    = &mysucceed,
	.transitionaction = NULL,
	.state = {
		{	// state 0 - expect: 'a'|'b'
			.narcs  = 3,
			.arc    = {
				{ 'a', 0 },
				{ 'b', 1 },
				{ DEFAULT_TOK, FAIL },
			},
		},
		{	// state 1: seen 'a'* 'b'+, expect: 'b'|'c'|'d'
			.narcs  = 4,
			.arc    = {
				{ 'b', 1 },
				{ 'c', 2 },
				{ 'd', 2 },
				{ DEFAULT_TOK, FAIL },
			},
		},
	},
};


#define NELEMS(arr)	(sizeof(arr)/sizeof(*arr))


// ---- 1-active-fsm tests, assuming the regex was anchored with ^ ----

// each test is a list of (nextch,nextstate) pairs, with 0 meaning failure
// and 3 meaning success

static char128 test1[] =
{
	"b1cS",		// b -> 1, c -> success
	"b1dS",		// b -> 1, d -> success
	"b1b1cS",	// b -> 1, b -> 1, c -> success
	"a0b1cS",	// a -> 0, b -> 1, c -> success
	"a0b1dS",	// a -> 0, b -> 1, d -> success
	"a0a0b1cS",	// a -> 0, a -> 0, b -> 1, c -> success
	"cF",		// c -> FAIL
	"dF",		// d -> FAIL
	"zF",		// z -> FAIL
	"b1b1",		// b -> 1, b -> 1
	"b1aF",		// b -> 1, a -> FAIL
	"b1xF",		// b -> 1, x -> FAIL
	"a0b1b1",	// a -> 0, b -> 1, b -> 1
	"a0b1aF",	// a -> 0, b -> 1, x -> FAIL
	"a0b1xF",	// a -> 0, b -> 1, x -> FAIL
	"a0a0b1xF",	// a -> 0, a -> 0, b -> 1, x -> FAIL
	"a0a0b1b1xF",	// a -> 0, a -> 0, b -> 1, b -> 1, x -> FAIL
	"a0a0a0a0b1cS",	// a, a, a, a -> 0, b -> 1, c -> success
	"a0a0a0a0b1dS",	// a, a, a, a -> 0, b -> 1, d -> success
	"a0a0a0b1b1dS",	// a, a, a -> 0, b -> 1, b -> 1, d -> success
	"a0a0a0a0b1xF",	// a, a, a, a -> 0, b -> 1, x -> FAIL
	"a0a0a0b1b1xF",	// a, a, a, a -> 0, b -> 1, b -> 1, x -> FAIL
};


static void dotest1( void )
{
	int nel = NELEMS(test1);
	for( int i=0; i<nel; i++ )
	{
		succeeded = false;

		printf( "test: %s ->\n", test1[i] );

		int len = strlen(test1[i]);
		assert( len%2 == 0 );

		FSM_fleet l = fsm_create_fleet( &myfsm, &new_mydata );
		assert( l->len == 1 );

		char256 label;

		// for each (ch, ns) in test1[i]
		for( int pos=0; pos<len; pos+=2 )
		{
			char ch  = test1[i][pos];
			char cns = test1[i][pos+1];
			int ns   = cns-'0';
			if( cns=='S' ) ns = 2;
			if( cns=='F' ) ns = -1;
			printf( "%s: pos=%d, ch=%c, ns=%d\n",
				test1[i], pos, ch, ns );
			fsm_update_fleet( l, ch );

			assert( l->len < 2 );

			sprintf( label, "test1(%s).after('%c'), newstate",
				test1[i], ch );
			if( ns == 2 && succeeded ) ns = -1;
			testint( l->actsm[0].currstate, ns, label );
		}

		fsm_destroy_fleet( l );

		if( test1[i][len-1] == 'S' )
		{
			sprintf( label, "test1(%s).succeeds", test1[i] );
			testcond( succeeded, label );
		} else
		{
			sprintf( label, "test1(%s).fails", test1[i] );
			testcond( !succeeded, label );
		}
	}
}


// multi-active-fsm tests, test cloning works to implement non-anchored

// each test is a triple (string, success, expected winning asmno)
// where the string is a disguised list of (nextch,#asms,each asm i:state)
// tuples, with state S meaning success (there are no Fs because failed
// asms get pruned out before we can check them)

typedef struct
{
	char128 str;
	bool    expectsuccess;
	int     expectasmno;
} mastest;

static mastest test2[] =
{
	// a
	{ "a1:0", false, -1 },

	// b
	{ "b1:1", false, -1 },

	// z
	{ "z0:", false, -1 },

	// abc
	{ "a1:0,b2:11,c0:", true, 1 },

	// ab
	{ "a1:0,b2:11", false, -1 },

	// az
	{ "a1:0,z0:", false, -1 },

	// bc
	{ "b1:1,c0:", true, 1 },

	// bd
	{ "b1:1,d0:", true, 1 },

	// bz
	{ "b1:1,z0:", false, -1 },

	// ababc
	{ "a1:0,b2:11,a1:0,b2:11,c0:", true, 3 },

	// azabc
	{ "a1:0,z0:,a1:0,b2:11,c0:", true, 3 },

	// halloababc
	{ "h0:,a1:0,l0:,l0:,o0:,a1:0,b2:11,a1:0,b2:11,c0:", true, 8 },
};


static void dotest2( void )
{
	int nel = NELEMS(test2);
	for( int i=0; i<nel; i++ )
	{
		char   *str           = test2[i].str;
		bool    expectsuccess = test2[i].expectsuccess;
		int     expectasmno   = test2[i].expectasmno;
		char   *origstr       = strdup(str);
		assert( origstr != NULL );
		char256 label;

		printf( "test: %s ->\n", str );

		FSM_fleet l = fsm_create_fleet( &myfsm, &new_mydata );
		assert( l->len == 1 );

		succeeded = false;
		char *tuple = str;

		// for each comma-separated tuple in str..
		// "h0:,a1:0,l0:,l0:,o0:,a1:0,b2:11,a1:0,b2:11,c0:",
		while( tuple != NULL && !succeeded )
		{
			char *comma = strchr(tuple,',');
			if( comma != NULL ) *comma = '\0';

			char *s = tuple;
			char ch = *s++;
			int n   = *s++;
			n -= '0';
			assert( *s++ == ':' );
			printf( "debug: test2: tuple=%s, ch=%c, n=%d, "
				"s=%s, beyond comma=%s\n",
				tuple, ch, n, s, comma==NULL?"":comma+1 );
			if( tuple>str )
			{
				printf( "debug: test2: adding new asm\n" );
				fsm_addto_fleet( l, 0, l->dc() );
			}
			assert( l->len > 0 );
			fsm_update_fleet( l, ch );
			sprintf( label, "test2(%s).tuple('%s').n",
				origstr, tuple );
			testint( l->len, n, label );

			// now test each state char in s (n of them)
			for( int i=0; i<n; i++ )
			{
				char cns = s[i];
				int  ns  = cns-'0';
				if( cns=='S' ) ns = 2;
				if( cns=='F' ) ns = -1;
				int got = l->actsm[i].currstate;
				printf( "debug: %s: ns=%d, i=%d, "
					"asm[%d].state=%d\n",
					s, ns, i, i, got );

				sprintf( label,
					"test2(%s).tuple('%s').asm[%d].state",
					origstr, tuple, i );

				testint( got, ns, label );
			}

			if( comma != NULL ) *comma = ',';
			tuple = comma != NULL ? comma+1 : NULL;
		}
		fsm_update_fleet( l, '\0' );

		fsm_destroy_fleet( l );

		if( expectsuccess )
		{
			sprintf( label, "test2(%s).succeeds", origstr );
			testcond( succeeded, label );

			sprintf( label, "test2(%s).succeed_asm", origstr );
			testint( winner, expectasmno, label );
			printf( "debug: winner=%d, expectasmno=%d\n",
				winner, expectasmno );

		} else
		{
			sprintf( label, "test2(%s).fails", origstr );
			testcond( !succeeded, label );
		}

		free( origstr );
	}
}


int main( void )
{
	dotest1();
	dotest2();
	return 0;
}
