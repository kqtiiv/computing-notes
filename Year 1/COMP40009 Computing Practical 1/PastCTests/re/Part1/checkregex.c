// checkregex.c:	check regex strings after parsing, to ban various
//			combinations that we've already rewritten.

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

#include "regex.h"
#include "checkregex.h"

#define CHECK_DEBUG
#undef CHECK_DEBUG


static bool badfltype[8] = {	// Banned first and last SimpleTypes
	false,	// Dot
	false,  // One
    	false,  // OptDot
	false,  // Opt
	true,   // DotStar
	false,  // Star
	true,   // DotPlus
	false,  // Plus
};


//
// char *mesg = check_first_or_last( sp, fl, re );
//	Check first or last SimplePat sp against the banned list.
//	fl is a string like "starts" or "ends" that is incorporated
//	into the message if necessary.
//	Return NULL if it's fine, or a strdup()ed message if not.
//
static char *check_first_or_last( SimplePat sp, char *fl, RE re )
{
	if( badfltype[ sp->t ] )
	{
		char256 mesg;
		char *s = mesg;
 		stringify_RE( s, re );
		s += strlen(s);
		sprintf( s, " %s with banned atom ", fl );
		s += strlen(s);
 		stringify_SimplePat( s, sp );
		return strdup(mesg);
	}
	return NULL;
}


// The ban matrix: matrix[kn1][kn+1] tells us
// if kn1 followed by kn+1 is a banned combination
// (that shouldn't occur because of simplification
// rewrites).  '.' means it's fine.  'B' means it's
// banned, and 'O' means it's banned if the two
// SimplePat set-strings contain a single char in
// common - otherwise it's ok.

static char *ban_matrix[8] =
{
	"....B...",	// Dot
	"........",	// One
	"B...B.B.",	// OptDot
	"........",	// Opt
	"B.B.B.B.",	// DotStar
	".O.O.O.O",     // Star
	"B.B.B.B.",	// DotPlus
	".O.O.O.O",     // Plus
};


//
// char dup = char_in_common( str1, str2 );
//	Return a duplicate char found in the two strings str1 and str2
//	(if they have one or more characters in common, i.e. considered
//	as sets they intersect, or '\0' if they have no char in common.
//
char char_in_common( char *a, char *b )
{
	// STUB: Question 1: implement this
	for (int i=0; i<strlen(a); i++) {
		char *found = strchr(b, a[i]);
		if (found != NULL) { return *found; }
	}

	return '\0';
}


//
// char *mesg = check_adjacent( l, nel, re );
//	Check all adjacent pairs of SimplePats in l[nel] against
//	the banned matrix. Return NULL if it's fine, or a strdup()ed
//	error message if not (the caller is responsible for free()ing
//	that error message in this case).
//
static char *check_adjacent( SPList l, int nel, RE re )
{
	for( int i=0; i<nel-1; i++ )
	{
		int k1 = l->l[i]->t;
		int k2 = l->l[i+1]->t;
		char c = ban_matrix[k1][k2];

		#ifdef CHECK_DEBUG
		printf( "debug: c_adj: i=%d, k1=%d, l[%d]=", i, k1, i );
		print_SimplePat( stdout, l->l[i] );
		printf( ", k2=%d, l[%d]=", k2, i );
		print_SimplePat( stdout, l->l[i+1] );
		printf( ": ban char = '%c'\n", c );
		#endif

		if( c == '.' ) continue;
		if( c == 'B' )
		{
			char256 mesg;
			char *s = mesg;
			stringify_RE( s, re );
			s += strlen(s);

			strcpy( s, " has banned combination " );
			s += strlen(s);

			stringify_SimplePat( s, l->l[i] );
			s += strlen(s);
			stringify_SimplePat( s, l->l[i+1] );
			s += strlen(s);

			return strdup(mesg);
		}
		if( c == 'O' )
		{
			char * s1 = l->l[i]->s;
			char * s2 = l->l[i+1]->s;
			char common = char_in_common( s1, s2 );
			if( common != '\0' )
			{
				char256 mesg;
				char *s = mesg;
				stringify_RE( s, re );
				s += strlen(s);
				sprintf( s, " has char '%c' in common in "
					    "adjacent ", common );
				s += strlen(s);
				stringify_SimplePat( s, l->l[i] );
				s += strlen(s);
				stringify_SimplePat( s, l->l[i+1] );
				s += strlen(s);

				return strdup(mesg);
			}
		} else
		{
			fprintf( stderr, 
				"Bad char '%c' (code %d) in ban matrix\n",
				c, c );
			exit(1);
		}
	}
	return NULL;
}


// char *error = check_regex( re );
//	Given a abstract RE re, check that it has no
//	banned combinations of SimplePats.  
//	Returns NULL if the RE is fine, or a strdup()ed
//	error message if not (the caller is responsible
//	for free()ing that error message in this case).
//
char *check_regex( RE re )
{
	#if CHECK_DEBUG
	printf( "debug: check_re(" );
	print_RE( stdout, re );
	printf( ")\n" );
	#endif

	SPList l   = re->l;
	int    nel = l->nel;

	if( nel == 0 )
	{
		#if CHECK_DEBUG
		printf( "debug: Empty RE is fine\n" );
		#endif
		return NULL;
	}

	char *mesg = check_first_or_last( l->l[0], "starts", re );
	if( mesg != NULL )
	{
		#if CHECK_DEBUG
		printf( "debug: RE fails first check with mesg '%s'\n", mesg );
		#endif
		return mesg;
	}

	if( nel>1 )
	{
		mesg = check_first_or_last( l->l[nel-1], "ends", re );
		if( mesg != NULL )
		{
			#if CHECK_DEBUG
			printf( "debug: RE fails last check with mesg '%s'\n",
				mesg );
			#endif
			return mesg;
		}

		mesg = check_adjacent( l, nel, re );
		if( mesg != NULL )
		{
			#if CHECK_DEBUG
			printf( "debug: RE fails adjacent checks with mesg '%s'\n",
				mesg );
			#endif
			return mesg;
		}
	}

	#if CHECK_DEBUG
	printf( "debug: check_re(" );
	print_RE( stdout, re );
	printf( "): is fine\n" );
	#endif

	return NULL;
}
