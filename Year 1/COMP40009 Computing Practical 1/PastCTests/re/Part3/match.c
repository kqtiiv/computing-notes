// match.c: regex pattern matching via an FSM

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "regex.h"
#include "fsm.h"
#include "match.h"
#include "token.h"
#include "emphasize.h"


#define MATCH_DEBUG
#undef MATCH_DEBUG


static int currpos    = 0;

// answer:
static bool matched   = false;
int matched_startpos  = 0;
int matched_endpos    = 0;
static char *matchstr = NULL;

// private data is int: position where the current match started


//
// highlight_match( startcol, line, startpos, endpos );
//	attempt to highlight the matching part (line[startpos..endpos]) given
//	that the line starts at column startcol
//
static void highlight_match( int startcol, char *line, int startpos, int endpos )
{
	assert( endpos >= startpos );

	int matchw = 1+endpos-startpos;

	#if 0
	printf( "debug: hm: startcol=%d, startpos=%d, endpos=%d, matchw=%d, "
		"line=<%s>\n", startcol, startpos, endpos, matchw, line );
	#endif

	char256 before;
	assert( startpos<sizeof(char256) );
	strncpy( before, line, startpos );
	before[startpos] = '\0';

	char256 matched;
	strncpy( matched, line+startpos, matchw );
	assert( matchw<sizeof(char256) );
	matched[matchw] = '\0';

	char256 after;
	*after = '\0';
	if( line[endpos] != '\0' )
	{
		assert( strlen(line+endpos+1)<sizeof(char256) );
		strcpy( after, line+endpos+1 );
	}

	#if 0
	printf( "debug: before=<%s>, matched=<%s>, after=<%s>\n",
		before, matched, after );
	//exit(1);
	#endif

	printf( "%s", before );
	emphasize( matched );
	printf( "%s\n", after );
}


#ifdef MATCH_DEBUG
static void matchone( int smno, int state, int nextch, int d )
{
	char matchch = nextch;
	printf( "FSM %d: have matched token (", smno );
	showtok( matchch, stdout );
	printf( ") against input: '%s', d=%d, currpos=%d\n", matchstr+currpos, d, currpos );
	highlight_match( 0, matchstr, d, currpos );
}
#endif


static void succeed( int smno, int state, int nextch, int d )
{
	matched_startpos = d;
	matched_endpos   = currpos;
	matched          = true;
	#ifdef MATCH_DEBUG
	printf( "debug: success: FSM %d, sp=%d, ep=%d\n", smno, matched_startpos, matched_endpos );
	#endif
}


// int d = new_mydata();
//	Create a new piece of FSM data, recording the current pos
//
static int new_mydata( void )
{
	return currpos;
}


//
// FSM fsm;
// int startpos, endpos;
// bool ismatched = match( flags, fsm, str, &startpos, &endpos );
//	Attempt to match str against the regex fsm and it's flags
//	(anchored at the start of the string if startanchor is true,
//	floating along the string otherwise)
//	return true iff the regex matches the string,
//	and in that case also set startpos and endpos.
//
bool match( REFlags flags, FSM *fsm, char *str, int *sp, int *ep )
{
	bool startanchor      = flags->startanchor;
	matchstr              = str;
	matched               = false;
	*sp                   = 0;
	*ep                   = 0;
	fsm->successaction    = &succeed;
        fsm->transitionaction = NULL;
	#ifdef MATCH_DEBUG
        fsm->transitionaction = &matchone;
	#endif

	currpos = 0;
	FSM_fleet l = fsm_create_fleet( fsm, &new_mydata );
	for( ; matchstr[currpos] != '\0' && !matched; currpos++ )
	{
		char t = matchstr[currpos];
		if( currpos>0 && ! startanchor )
		{
			fsm_addto_fleet( l, 0, l->dc() );
		}
		fsm_update_fleet( l, t );
	}
	fsm_update_fleet( l, '\0' );

	fsm_destroy_fleet( l );

	if( matched )
	{
		*sp = matched_startpos;
		*ep = matched_endpos;

		if( ! flags->endanchor && flags->endinoptional )
		{
			(*ep)--;
		}
		if( flags->backtostart )
		{
			*sp = 0;
		}
		if( flags->ontoend )
		{
			*ep = strlen(str)-1;
		}
	}

	return matched;
}


//
// report_match_str( regex_flags, &myfsm, searchstr, regex );
//	Perform a match - from a string not a file - and report on the results
//
void report_match_str( REFlags flags, FSM *fsm, char *searchstr, char *regex )
{
	#if 0
	printf( "debug: rms: searchstr=<%s>\n", searchstr );
	#endif
	int startpos, endpos;
	bool matched = match( flags, fsm, searchstr, &startpos, &endpos );
	#if 0
	printf( "debug: rms after match: searchstr=<%s>\n", searchstr );
	#endif
	if( matched )
	{
		printf( "have matched RE /%s/ against target: ", regex );
		highlight_match( 0, searchstr, startpos, endpos );
	} else
	{
		printf( "no match for RE /%s/ against target: %s\n",
			regex, searchstr );
	}
}


//
// report_match_line( flags, &fsm, line, filename, showlabel, lineno );
//	Perform a match of <line> against the compiled RE FSM <fsm>,
//	and report upon the results.  <filename>, <showlabel> and <lineno>
//	are used in reporting the results
//
void report_match_line( REFlags flags, FSM *fsm, char *line, char *filename, bool showlabel, int lineno )
{
	int startpos, endpos;
	bool matched = match( flags, fsm, line, &startpos, &endpos );
	if( ! matched ) return;

	int w = 0;

	if( showlabel )
	{
		char256 label;
		if( filename != NULL )
		{
			sprintf( label, "%s:%d", filename, lineno );
		}
		else
		{
			sprintf( label, "%d", lineno );
		}
		w = strlen(label)+1;
		printf( "%s ", label );
	}
	highlight_match( w, line, startpos, endpos );
}
