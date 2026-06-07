// testmakefsm: test the makefsm module (RE to FSM conversion)..

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <assert.h>

#include "testutils.h"

#include "regex.h"
#include "fsm.h"
#include "token.h"
#include "makefsm.h"


#define NELEMS(arr)	(sizeof(arr)/sizeof(*arr))


// FSM fsm;
// createfsm( &fsm, nstates );
//	Make fsm a completely empty FSM with nstates states
//
static void createfsm( FSM *fsm, int ns )
{
	fsm->nstates         = ns;
	for( int i=0; i<ns; i++ )
	{
		fsm->state[i].narcs  = 0;
	}
	fsm->successstate    = -1;
	fsm->preservestate0  = false; // do not preserve state 0 ASMs
}


// FSM     fsm;  (then initialize it)
// char512 fsmstr;
// stringify_fsm( fsmstr, &fsm );
//	Convert FSM fsm into a conveniently formatted string fsmstr
//	(of the form 0:tok->ns,tok2->ns2...,DEFAULT->nsd 1:tok...
//
static void stringify_fsm( char *fsmstr, FSM *fsm )
{
	char *s = fsmstr;
	*s = '\0';
	for( int i=0; i<fsm->nstates; i++ )
	{
		FSM_state *state = &fsm->state[i];
		sprintf( s, "%d:", i );
		s += strlen(s);
		int	 narcs = state->narcs;
		FSM_arc *arc   = state->arc;
		for( int j=0; j<narcs; j++, arc++ )
		{
			if( j>0 )
			{
				*s++ = ',';
				*s   = '\0';
			}
			sprintf( s, "%s%d", gettokstr(arc->tok), arc->nextstate );
			s += strlen(s);
		}
		*s++ = ' ';
		*s   = '\0';
	}
}



// --- tests for arc() and handleset() ---


typedef struct {
				// add a new arc or handle:
	int	sno;		// state no: 0 or 1 in these tests
	char   *set;		// set of 1 or more printable chars - tokens
	int	ns;		// nextstate the arc/set goes into
	char   *expected;	// expected string, format sno:tok->ns, ..
} arctest;


static arctest arc_tests[] =
{
	{ 0, "a", 0, "0:a0 1: " },
	{ 0, "b", 1, "0:a0,b1 1: " },
	{ 0, "c", 1, "0:a0,b1,c1 1: " },
	{ 0, "ijk", 2, "0:a0,b1,c1,i2,j2,k2 1: " },
	{ 1, "abc", 0, "0:a0,b1,c1,i2,j2,k2 1:a0,b0,c0 " },
	{ 1, "x", 0, "0:a0,b1,c1,i2,j2,k2 1:a0,b0,c0,x0 " },
	{ 1, "y", 1, "0:a0,b1,c1,i2,j2,k2 1:a0,b0,c0,x0,y1 " },
	{ 1, "z", 2, "0:a0,b1,c1,i2,j2,k2 1:a0,b0,c0,x0,y1,z2 " },
	{ 0, "pqr", 0, "0:a0,b1,c1,i2,j2,k2,p0,q0,r0 1:a0,b0,c0,x0,y1,z2 " },
};


static void testarc( void )
{
	FSM fsm;
	createfsm( &fsm, 2 );

	for( int i=0; i<NELEMS(arc_tests); i++ )
	{
		arctest   *t       = &arc_tests[i];
		char      *s       = t->set;
		FSM_state *state   = &fsm.state[t->sno];
		char      *descrip = "arc";
		if( s[1] == '\0' )
		{
			// one token so one arc
			arc( state, s[0], t->ns );
		} else
		{
			// set of tokens
			arc_set( state, s, t->ns );
			descrip = "set";
		}
		printf( "testarc/set: +%s(%d, '%s' -> %d)\n",
			descrip, t->sno, t->set, t->ns );

		char512 fsmstr;
		stringify_fsm( fsmstr, &fsm );

		char256 label;
		sprintf( label, "+%s(%d,%s,%d).str(%s)",
			descrip, t->sno, t->set, t->ns, t->expected );
		teststring( fsmstr, t->expected, label );
	}
}


// --- append_spat tests ---


typedef struct {
	SimpleType t;
	optstr	   s;
	char      *output;	// stringify(append_spat(make_SimplePat(t,s)))
} asptest;


#define STR0 "0:ANY1,DEFAULT-1 "
#define STR1 "1:a2,DEFAULT-1 "
#define STR2 "2:a3,b3,c3,DEFAULT-1 "
#define STR3 "3:ANY4,UNLABELLED4 "
#define STR4 "4:a5,UNLABELLED5 "
#define STR5 "5:a6,b6,c6,UNLABELLED6 "
#define STR6 "6:ANY6,UNLABELLED7 "
#define STR7 "7:a7,UNLABELLED8 "
#define STR8 "8:a8,b8,c8,UNLABELLED9 "
#define STR9 "9:ANY10,DEFAULT-1 10:ANY10,UNLABELLED11 "
#define STR11 "11:a12,DEFAULT-1 12:a12,UNLABELLED13 "
#define STR13 "13:a14,b14,c14,DEFAULT-1 14:a14,b14,c14,UNLABELLED15 "

static asptest appspat_tests[] =
{
	{ SimpleType_is_Dot,     NULL,  STR0 "1: " },
	{ SimpleType_is_One,     "a",   STR1 "2: " },
	{ SimpleType_is_One,     "abc", STR2 "3: " },
	{ SimpleType_is_OptDot,  NULL,  STR3 "4: " },
	{ SimpleType_is_Opt,     "a",   STR4 "5: " },
	{ SimpleType_is_Opt,     "abc", STR5 "6: " },
	{ SimpleType_is_DotStar, NULL,  STR6 "7: " },
	{ SimpleType_is_Star,    "a",   STR7 "8: " },
	{ SimpleType_is_Star,    "abc", STR8 "9: " },
	{ SimpleType_is_DotPlus, NULL,  STR9 "11: " },
	{ SimpleType_is_Plus,    "a",   STR11 "13: " },
	{ SimpleType_is_Plus,    "abc", STR13 "15: "
	},
};


static void testappspat( void )
{
	FSM fsm;
	createfsm( &fsm, 30 );
	fsm.nstates = 1;

	char512  expected;
	strcpy( expected, "0: " );

	for( int i=0; i<NELEMS(appspat_tests); i++ )
	{
		asptest *t = &appspat_tests[i];
		//printf( "testappspat(t=%d, s='%s')",
		//	  t->t, t->s==NULL?"null":t->s );

		SimplePat spat = make_SimplePat( t->t, t->s );
		append_spat( spat, &fsm );

		char512 output;
		stringify_fsm( output, &fsm );

		// update expected
		char *e = expected+strlen(expected)-1;
		assert( *e-- == ' ' );
		assert( *e-- == ':' );
		while( isdigit(*e) ) e--;
		e++;
		assert( strncmp( e, t->output, strlen(e)-1 ) == 0 );
		#if 0
		if( strncmp( e, t->output, strlen(e)-1 ) != 0 )
		{
			fprintf( stderr, "debug error: expected=%s, e=%s, to=%s\n",
				expected, e, t->output );
			exit(1);
		}
		#endif
		strcpy( e, t->output );

		char256 label;
		char128 ssp;
		stringify_SimplePat( ssp, spat );
		sprintf( label, "app_spat(%s)", ssp );
		teststring( output, expected, label );

		free_SimplePat( spat );
	}
}


// --- phase tests ---


typedef struct {
	struct SimplePat list[30];
	char      *phase1fsm;
	bool       dophase2, dophase3, dophase4;
	char      *phase2fsmwanchor;
	char      *phase2fsmwoanchor;
	char      *phase3fsmwanchor;
	char      *phase3fsmwoanchor;
	char      *phase4fsmwanchor;
	char      *phase4fsmwoanchor;
} spltest;


#define TESTNUL(n,m) #n ":NUL" #m ",DEFAULT-1 "


// "a"
static spltest just_a = {
	.list = {
		{ SimpleType_is_One, "a" },
		{ -1, NULL },
	},
	#define XXX "0:a1,DEFAULT-1 "
	.phase1fsm         = XXX,
	.dophase2          = true,
	.phase2fsmwanchor  = XXX TESTNUL(1,2),
	.phase2fsmwoanchor = XXX,
	.dophase3          = true,
	.phase3fsmwanchor  = XXX TESTNUL(1,2),
	.phase3fsmwoanchor = XXX,
	.dophase4          = true,
	.phase4fsmwanchor  = XXX TESTNUL(1,2),
	.phase4fsmwoanchor = XXX,
	#undef XXX
};


// "hello"
static spltest hello = {
	.list = {
		{ SimpleType_is_One, "h" },
		{ SimpleType_is_One, "e" },
		{ SimpleType_is_One, "l" },
		{ SimpleType_is_One, "l" },
		{ SimpleType_is_One, "o" },
		{ -1, NULL },
	},
	#define XXX "0:h1,DEFAULT-1 1:e2,DEFAULT-1 2:l3,DEFAULT-1 " \
		    "3:l4,DEFAULT-1 4:o5,DEFAULT-1 "
	.phase1fsm         = XXX,
	.dophase2          = true,
	.phase2fsmwanchor  = XXX TESTNUL(5,6),
	.phase2fsmwoanchor = XXX,
	.dophase3          = true,
	.phase3fsmwanchor  = XXX TESTNUL(5,6),
	.phase3fsmwoanchor = XXX,
	.dophase4          = true,
	.phase4fsmwanchor  = XXX TESTNUL(5,6),
	.phase4fsmwoanchor = XXX,
	#undef XXX
};


// same eg as append_spat tests build up too, all in one
static spltest one = {
	.list = {
		{ SimpleType_is_Dot,     NULL },
		{ SimpleType_is_One,     "a" },
		{ SimpleType_is_One,     "abc" },
		{ SimpleType_is_OptDot,  NULL },
		{ SimpleType_is_Opt,     "a" },
		{ SimpleType_is_Opt,     "abc" },
		{ SimpleType_is_DotStar, NULL },
		{ SimpleType_is_Star,    "a" },
		{ SimpleType_is_Star,    "abc" },
		{ SimpleType_is_DotPlus, NULL },
		{ SimpleType_is_Plus,    "a" },
		{ SimpleType_is_Plus,    "abc" },
		{ -1, NULL },
	},
	.phase1fsm = STR0 STR1 STR2 STR3 STR4 STR5 STR6
		     STR7 STR8 STR9 STR11 STR13,
};


// ".*"
static spltest dotstar = {
	.list = {
		{ SimpleType_is_DotStar, NULL },
		{ -1, NULL },
	},
	#define XXX(ud)    "0:ANY0," ud "1 "
	.phase1fsm         = XXX("UNLABELLED"),
	.dophase2          = true,
	.phase2fsmwanchor  = XXX("UNLABELLED") TESTNUL(1,2),
	.phase2fsmwoanchor = XXX("DEFAULT"),
	.dophase3          = true,
	.phase3fsmwanchor  = "0:NUL2,ANY0,DEFAULT-1 1:NUL2,DEFAULT-1 ",
	.phase3fsmwoanchor = "0:ANY0,DEFAULT1 ",
	.dophase4          = true,
	.phase4fsmwanchor  = "0:NUL1,ANY0,DEFAULT-1 ",
	.phase4fsmwoanchor = "0:ANY0,DEFAULT1 ",
	#undef XXX
};


// ".*a"
static spltest dotstar_a = {
	.list = {
		{ SimpleType_is_DotStar, NULL },
		{ SimpleType_is_One, "a" },
		{ -1, NULL },
	},
	#define XXX        "0:ANY0,UNLABELLED1 1:a2,DEFAULT-1 "
	#define YYY        "0:a2,ANY0,DEFAULT-1 1:a2,DEFAULT-1 "
	#define ZZZ        "0:a1,ANY0,DEFAULT-1 "
	.phase1fsm         = XXX,
	.dophase2          = true,
	.phase2fsmwanchor  = XXX TESTNUL(2,3),
	.phase2fsmwoanchor = XXX,
	.dophase3          = true,
	.phase3fsmwanchor  = YYY TESTNUL(2,3),
	.phase3fsmwoanchor = YYY,
	.dophase4          = true,
	.phase4fsmwanchor  = ZZZ TESTNUL(1,2),
	.phase4fsmwoanchor = ZZZ,
	#undef XXX
	#undef YYY
	#undef ZZZ
};


// "b.*"
static spltest b_dotstar = {
	.list = {
		{ SimpleType_is_One, "b" },
		{ SimpleType_is_DotStar, NULL },
		{ -1, NULL },
	},
	#define XXX(ud)    "0:b1,DEFAULT-1 1:ANY1," ud "2 "
	#define YYY	   "0:b1,DEFAULT-1 1:NUL2,ANY1,DEFAULT-1 "
	.phase1fsm         = XXX("UNLABELLED"),
	.dophase2          = true,
	.phase2fsmwanchor  = XXX("UNLABELLED") TESTNUL(2,3),
	.phase2fsmwoanchor = XXX("DEFAULT"),
	.dophase3          = true,
	.phase3fsmwanchor  = "0:b1,DEFAULT-1 1:NUL3,ANY1,DEFAULT-1 2:NUL3,DEFAULT-1 ",
	.phase3fsmwoanchor = XXX("DEFAULT"),
	.dophase4          = true,
	.phase4fsmwanchor  = YYY,
	.phase4fsmwoanchor = XXX("DEFAULT"),
	#undef XXX
	#undef YYY
};


// "a*"
static spltest a_star = {
	.list = {
		{ SimpleType_is_Star, "a" },
		{ -1, NULL },
	},
	#define XXX(ud)    "0:a0," ud "1 "
	.phase1fsm         = XXX("UNLABELLED"),
	.dophase2          = true,
	.phase2fsmwanchor  = XXX("UNLABELLED") TESTNUL(1,2),
	.phase2fsmwoanchor = XXX("DEFAULT"),
	.dophase3          = true,
	.phase3fsmwanchor  = "0:NUL2,a0,DEFAULT-1 " TESTNUL(1,2),
	.phase3fsmwoanchor = XXX("DEFAULT"),
	.dophase4          = true,
	.phase4fsmwanchor  = "0:NUL1,a0,DEFAULT-1 ",
	.phase4fsmwoanchor = XXX("DEFAULT"),
	#undef XXX
};


// { "a+"
static spltest a_plus = {
	.list = {
		{ SimpleType_is_Plus, "a" },
		{ -1, NULL },
	},
	#define ZERO         "0:a1,DEFAULT-1 "
	#define XXX(ud)      ZERO "1:a1," ud "2 "
	#define YYY(n)	     ZERO "1:NUL" #n ",a1,DEFAULT-1 "
	.phase1fsm         = XXX("UNLABELLED"),
	.dophase2          = true,
	.phase2fsmwanchor  = XXX("UNLABELLED") TESTNUL(2,3),
	.phase2fsmwoanchor = XXX("DEFAULT"),
	.dophase3          = true,
	.phase3fsmwanchor  = YYY(3) TESTNUL(2,3),
	.phase3fsmwoanchor = XXX("DEFAULT"),
	.dophase4          = true,
	.phase4fsmwanchor  = YYY(2),
	.phase4fsmwoanchor = XXX("DEFAULT"),
	#undef ZERO
	#undef XXX
	#undef YYY
};



// { ".+"
static spltest dotplus = {
	.list = {
		{ SimpleType_is_DotPlus, NULL },
		{ -1, NULL },
	},
	#define ZERO        "0:ANY1,DEFAULT-1 "
	#define XXX         ZERO "1:ANY1,UNLABELLED2 "
	#define YYY         ZERO "1:ANY1,DEFAULT2 "
	#define ZZZ(n)      ZERO "1:NUL" #n ",ANY1,DEFAULT-1 "
	.phase1fsm         = XXX,
	.dophase2          = true,
	.phase2fsmwanchor  = XXX TESTNUL(2,3),
	.phase2fsmwoanchor = YYY,
	.dophase3          = true,
	.phase3fsmwanchor  = ZZZ(3) TESTNUL(2,3),
	.phase3fsmwoanchor = YYY,
	.dophase4          = true,
	.phase4fsmwanchor  = ZZZ(2),
	.phase4fsmwoanchor = YYY,
	#undef ZERO
	#undef XXX
	#undef YYY
	#undef ZZZ
};


// { ".+hi"
static spltest dotplus_hi = {
	.list = {
		{ SimpleType_is_DotPlus, NULL },
		{ SimpleType_is_One, "h" },
		{ SimpleType_is_One, "i" },
		{ -1, NULL },
	},
	#define ZERO       "0:ANY1,DEFAULT-1 "
	#define XXX        ZERO "1:ANY1,UNLABELLED2 2:h3,DEFAULT-1 " \
			   "3:i4,DEFAULT-1 "
	#define YYY        ZERO "1:h3,ANY1,DEFAULT-1 2:h3,DEFAULT-1 " \
			   "3:i4,DEFAULT-1 "
	#define ZZZ        ZERO "1:h2,ANY1,DEFAULT-1 2:i3,DEFAULT-1 "
	.phase1fsm         = XXX,
	.dophase2          = true,
	.phase2fsmwanchor  = XXX TESTNUL(4,5),
	.phase2fsmwoanchor = XXX,
	.dophase3          = true,
	.phase3fsmwanchor  = YYY TESTNUL(4,5),
	.phase3fsmwoanchor = YYY,
	.dophase4          = true,
	.phase4fsmwanchor  = ZZZ TESTNUL(3,4),
	.phase4fsmwoanchor = ZZZ,
	#undef ZERO
	#undef XXX
	#undef YYY
	#undef ZZZ
};



// { "hi.+"
static spltest hi_dotplus = {
	.list = {
		{ SimpleType_is_One, "h" },
		{ SimpleType_is_One, "i" },
		{ SimpleType_is_DotPlus, NULL },
		{ -1, NULL },
	},
	#define AAA          "0:h1,DEFAULT-1 1:i2,DEFAULT-1 2:ANY3,DEFAULT-1 "
	#define XXX          AAA "3:ANY3,UNLABELLED4 "
	#define YYY          AAA "3:ANY3,DEFAULT4 "
	.phase1fsm         = XXX,
	.dophase2          = true,
	.phase2fsmwanchor  = XXX TESTNUL(4,5),
	.phase2fsmwoanchor = YYY,
	.dophase3          = true,
	.phase3fsmwanchor  = AAA "3:NUL5,ANY3,DEFAULT-1 4:NUL5,DEFAULT-1 ",
	.phase3fsmwoanchor = YYY,
	.dophase4          = true,
	.phase4fsmwanchor  = AAA "3:NUL4,ANY3,DEFAULT-1 ",
	.phase4fsmwoanchor = YYY,
	#undef YYY
	#undef XXX
	#undef AAA
};


// { "[a-dp-tq]+"
static spltest a2dp2tplus = {
	.list = {
		{ SimpleType_is_Plus, "abcdpqrst" },
		{ -1, NULL },
	},
	#define AT1          "a1,b1,c1,d1,p1,q1,r1,s1,t1,"
	#define ZERO         "0:" AT1 "DEFAULT-1 "
	#define XXX          ZERO "1:" AT1 "UNLABELLED2 "
	#define YYY          ZERO "1:" AT1 "DEFAULT2 "
	.phase1fsm         = XXX,
	.dophase2          = true,
	.phase2fsmwanchor  = XXX TESTNUL(2,3),
	.phase2fsmwoanchor = YYY,
	.dophase3          = true,
	.phase3fsmwanchor  = ZERO "1:NUL3," AT1 "DEFAULT-1 " TESTNUL(2,3),
	.phase3fsmwoanchor = YYY,
	.dophase4          = true,
	.phase4fsmwanchor  = ZERO "1:NUL2," AT1 "DEFAULT-1 ",
	.phase4fsmwoanchor = YYY,
	#undef AT1
	#undef ZERO
	#undef XXX
	#undef YYY
};


// { "a*b[ch]"
static spltest astar_b_c_or_h = {
	.list = {
		{ SimpleType_is_Star, "a" },
		{ SimpleType_is_One, "b" },
		{ SimpleType_is_One, "ch" },
		{ -1, NULL },
	},
	#define XXX        "0:a0,UNLABELLED1 1:b2,DEFAULT-1 2:c3,h3,DEFAULT-1 "
	#define YYY        "0:a0,b2,DEFAULT-1 1:b2,DEFAULT-1 2:c3,h3,DEFAULT-1 "
	#define ZZZ        "0:a0,b1,DEFAULT-1 1:c2,h2,DEFAULT-1 "
	.phase1fsm         = XXX,
	.dophase2          = true,
	.phase2fsmwanchor  = XXX TESTNUL(3,4),
	.phase2fsmwoanchor = XXX,
	.dophase3          = true,
	.phase3fsmwanchor  = YYY TESTNUL(3,4),
	.phase3fsmwoanchor = YYY,
	.dophase4          = true,
	.phase4fsmwanchor  = ZZZ TESTNUL(2,3),
	.phase4fsmwoanchor = ZZZ,
	#undef XXX
	#undef YYY
	#undef ZZZ
};


// { "a+b[ch]"
static spltest aplus_b_c_or_h = {
	.list = {
		{ SimpleType_is_Plus, "a" },
		{ SimpleType_is_One, "b" },
		{ SimpleType_is_One, "ch" },
		{ -1, NULL },
	},
	#define XXX          "0:a1,DEFAULT-1 1:a1,UNLABELLED2 " \
			     "2:b3,DEFAULT-1 3:c4,h4,DEFAULT-1 "
	#define YYY          "0:a1,DEFAULT-1 1:a1,b3,DEFAULT-1 " \
			     "2:b3,DEFAULT-1 3:c4,h4,DEFAULT-1 "
	#define ZZZ          "0:a1,DEFAULT-1 1:a1,b2,DEFAULT-1 " \
			     "2:c3,h3,DEFAULT-1 "
	.phase1fsm         = XXX,
	.phase1fsm         = XXX,
	.dophase2          = true,
	.phase2fsmwanchor  = XXX TESTNUL(4,5),
	.phase2fsmwoanchor = XXX,
	.dophase3          = true,
	.phase3fsmwanchor  = YYY TESTNUL(4,5),
	.phase3fsmwoanchor = YYY,
	.dophase4          = true,
	.phase4fsmwanchor  = ZZZ TESTNUL(3,4),
	.phase4fsmwoanchor = ZZZ,
	#undef XXX
	#undef YYY
	#undef ZZZ
};


// { "a?b"
static spltest opta_b = {
	.list = {
		{ SimpleType_is_Opt, "a" },
		{ SimpleType_is_One, "b" },
		{ -1, NULL },
	},
	#define XXX          "0:a1,UNLABELLED1 1:b2,DEFAULT-1 "
	#define YYY          "0:a1,b2,DEFAULT-1 1:b2,DEFAULT-1 "
	.phase1fsm         = XXX,
	.dophase2          = true,
	.phase2fsmwanchor  = XXX TESTNUL(2,3),
	.phase2fsmwoanchor = XXX,
	.dophase3          = true,
	.phase3fsmwanchor  = YYY TESTNUL(2,3),
	.phase3fsmwoanchor = YYY,
	.dophase4          = true,
	.phase4fsmwanchor  = YYY TESTNUL(2,3),
	.phase4fsmwoanchor = YYY,
	#undef XXX
	#undef YYY
};


// { "[a-f]?g"
static spltest opta2f_g = {
	.list = {
		{ SimpleType_is_Opt, "abcdef" },
		{ SimpleType_is_One, "g" },
		{ -1, NULL },
	},
	#define XXX        "0:a1,b1,c1,d1,e1,f1,UNLABELLED1 1:g2,DEFAULT-1 "
	#define YYY        "0:a1,b1,c1,d1,e1,f1,g2,DEFAULT-1 1:g2,DEFAULT-1 "
	.phase1fsm         = XXX,
	.dophase2          = true,
	.phase2fsmwanchor  = XXX TESTNUL(2,3),
	.phase2fsmwoanchor = XXX,
	.dophase3          = true,
	.phase3fsmwanchor  = YYY TESTNUL(2,3),
	.phase3fsmwoanchor = YYY,
	.dophase4          = true,
	.phase4fsmwanchor  = YYY TESTNUL(2,3),
	.phase4fsmwoanchor = YYY,
	#undef XXX
	#undef YYY
};


// { ".?[bc]"
static spltest optdot_borc = {
	.list = {
		{ SimpleType_is_OptDot, NULL },
		{ SimpleType_is_One, "bc" },
		{ -1, NULL },
	},
	#define XXX        "0:ANY1,UNLABELLED1 1:b2,c2,DEFAULT-1 "
	#define YYY        "0:b2,c2,ANY1,DEFAULT-1 1:b2,c2,DEFAULT-1 "
	.phase1fsm         = XXX,
	.dophase2          = true,
	.phase2fsmwanchor  = XXX TESTNUL(2,3),
	.phase2fsmwoanchor = XXX,
	.dophase3          = true,
	.phase3fsmwanchor  = YYY TESTNUL(2,3),
	.phase3fsmwoanchor = YYY,
	.dophase4          = true,
	.phase4fsmwanchor  = YYY TESTNUL(2,3),
	.phase4fsmwoanchor = YYY,
	#undef XXX
	#undef YYY
};


// { "r.+"
static spltest r_dotplus = {
	.list = {
		{ SimpleType_is_One, "r" },
		{ SimpleType_is_DotPlus, NULL },
		{ -1, NULL },
	},
	#define ZEROONE      "0:r1,DEFAULT-1 1:ANY2,DEFAULT-1 "
	#define XXX          ZEROONE "2:ANY2,UNLABELLED3 "
	#define YYY          ZEROONE "2:ANY2,DEFAULT3 "
	#define ZZZ(n)       ZEROONE "2:NUL" #n ",ANY2,DEFAULT-1 "
	.phase1fsm         = XXX,
	.dophase2          = true,
	.phase2fsmwanchor  = XXX TESTNUL(3,4),
	.phase2fsmwoanchor = YYY,
	.dophase3          = true,
	.phase3fsmwanchor  = ZZZ(4) TESTNUL(3,4),
	.phase3fsmwoanchor = YYY,
	.dophase4          = true,
	.phase4fsmwanchor  = ZZZ(3),
	.phase4fsmwoanchor = YYY,
	#undef ZEROONE
	#undef XXX
	#undef YYY
	#undef ZZZ
};


// testphases( test, testname, anchor );
//	Initially test phase1(), then (if phase2 expected fields are
//      set) then test phase2 as well, then phase 3 as well, with an end
//	anchor iff anchor is true.
//
static void testphases( spltest *test, char *testname, bool anchor )
{
	SPList l = make_SPList( 20 );
	for( int i=0; test->list[i].t != -1; i++ )
	{
		SimplePat p = &test->list[i];
		SimplePat spat = make_SimplePat( p->t, p->s );
		push_SPList( l, spat );
	}
	struct REFlags flags;
	init_REFlags( &flags );
	RE re = make_RE( &flags, l );

	FSM fsm;
	createfsm( &fsm, 30 );
	fsm.nstates = 1;

	phase1( re, &fsm );

	char512 output;
	stringify_fsm( output, &fsm );

	char256 label;
	sprintf( label, "%s.phase1", testname );
	teststring( output, test->phase1fsm, label );

	if( test->dophase2 )
	{
		re->flags->endanchor = anchor;
		phase2( re, &fsm );

		stringify_fsm( output, &fsm );

		char *with = anchor ? "with" : "without";
		sprintf( label, "%s.phase2 %s end anchor", testname, with );
		teststring( output,
			    anchor ? test->phase2fsmwanchor
			           : test->phase2fsmwoanchor,
			    label );

		if( test->dophase3 )
		{
			phase3( re, &fsm );

			stringify_fsm( output, &fsm );
			sprintf( label, "%s.phase3 %s end anchor",
				testname, with );
			teststring( output,
				    anchor ? test->phase3fsmwanchor
				    	   : test->phase3fsmwoanchor,
				label );

			if( test->dophase4 )
			{
				phase4( re, &fsm );

				stringify_fsm( output, &fsm );
				sprintf( label, "%s.phase4 %s end anchor",
					testname, with );
				teststring( output,
					    anchor ? test->phase4fsmwanchor
						   : test->phase4fsmwoanchor,
					label );
			}
		}
	}

	free_RE( re );
}


#define TPH(x) testphases( &x, #x, false ); testphases( &x, #x, true )


int main( void )
{
	testarc();
	testappspat();
	TPH( just_a );
	TPH( hello );
	TPH( a_star );
	TPH( a_plus );
	TPH( dotstar );
	TPH( dotstar_a );
	TPH( b_dotstar );
	TPH( dotplus );
	TPH( dotplus_hi );
	TPH( hi_dotplus );
	TPH( one );
	TPH( a2dp2tplus );
	TPH( astar_b_c_or_h );
	TPH( aplus_b_c_or_h );
	TPH( opta_b );
	TPH( opta2f_g );
	TPH( optdot_borc );
	TPH( r_dotplus );
	return 0;
}
