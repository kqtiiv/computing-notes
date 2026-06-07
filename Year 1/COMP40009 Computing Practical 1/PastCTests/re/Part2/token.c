// token.c:	token-related functions, especially acceptor logic

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

#include "regex.h"
#include "token.h"

typedef char char20[20];
static char20 tokstr[ UNLABELLED_TOK+1 ];
static bool token_init = false;


// setup_token();
//	Setup the tokstr[] array for nice display of tokens
//
static void setup_token( void )
{
	strcpy( tokstr[0], "NUL" );
	for( int i=1; i<32; i++ )
	{
		if( i<27 )
		{
			sprintf( tokstr[i], "ctrl-%c - code %d",
				 'A'+i-1, i );
		} else
		{
			sprintf( tokstr[i], "code %d", i );
		}
	}
	for( int i=32; i<128; i++ )
	{
		sprintf( tokstr[i], "%c", i );
		//sprintf( tokstr[i], "'%c' - code %d", i, i );
	}
	for( int i=128; i<256; i++ )
	{
		sprintf( tokstr[i], "code %d", i );
	}
	strcpy( tokstr[ANY_TOK], "ANY" );
	strcpy( tokstr[DEFAULT_TOK], "DEFAULT" );
	strcpy( tokstr[UNLABELLED_TOK], "UNLABELLED" );
}


// showtok( tok, out );
//	Display the given token to the given open file.
//
void showtok( int tok, FILE *out )
{
	if( ! token_init )
	{
		setup_token();
		token_init = true;
	}
	fprintf( out, "%s", tokstr[tok] );
}


// char *tokstr = gettokstr( tok );
//	Return a readonly token string corresponding to token.
//
char * gettokstr( int tok )
{
	if( ! token_init )
	{
		setup_token();
		token_init = true;
	}
	return tokstr[tok];
}


// bool ok = accepttok( arctok, tok, alreadymatched );
//	An arc wants to see arctok, and we actually see tok,
//	whether or not we've already matched an arc (alreadymatched) -
//	is this a matching arc (for find_matching_arcs)?
//
//	This is where custom application specific (in this case,
//	regex specific) token comparisons are done.  ANY_TOK must
//	match any non-NUL character, and DEFAULT_TOK must match
//	any character (NUL or non-NUL) as long as nothing else has
//	already matched.  Otherwise tokens have to match exactly.
//
bool accepttok( int arctok, int tok, bool alreadymatched )
{
	if( arctok == ANY_TOK ) return tok != '\0';
	if( arctok == DEFAULT_TOK && !alreadymatched ) return true;
	return arctok == tok;
}
