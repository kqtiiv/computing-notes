// testcheck: test the RE checking (simplify->parse->check)..

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

#include "testutils.h"

#include "regex.h"
#include "parseregex.h"
#include "simplifyregex.h"
#include "checkregex.h"


// generate a 5-bit binary string of flags..

#define ONEFLAG(val)   *s++ = flags->val?'1':'0';

static void flag2str( REFlags flags, char *flagstr )
{
	char *s = flagstr;
	ONEFLAG( startanchor );
	ONEFLAG( endanchor );
	ONEFLAG( endinoptional );
	ONEFLAG( backtostart );
	ONEFLAG( ontoend );
	*s = '\0';
}


typedef struct {
	char128 str;
	char128 answer;
	char128 flagstr;
	char128 regen;
} testquad;


#define NELEMS(arr)	(sizeof(arr)/sizeof(*arr))


// ---- good tests ----


static testquad oktests[] =
{
	{ "^.*",         "",              "00010", "" },
	{ "^.*a",        "a",             "00010", "" },
	{ "^.*.*a",      "a",             "00010", "" },
	{ "^.*a.*$",     "a",             "00011", "" },
	{ "^.*$",        "^",             "10001", "" },
	{ "b.*$",        "b",             "00001", "" },
	{ "^.+$",        "^.",            "10001", "" },
	{ "^.+hi",       ".hi",           "00010", "" },
	{ "ho.+$",       "ho.",           "00001", "" },
	{ "^hello",      "^hello",        "10000", "" },
	{ "^.*hello",    "hello",         "00010", "" },
	{ "^.+hello",    ".hello",        "00010", "" },
	{ "hello.*$",    "hello",         "00001", "" },
	{ "hello.+$",    "hello.",        "00001", "" },
	{ "^a*",         "^a*",           "10100", "" },
	{ "a*$",         "a*$",           "01100", "" },
	{ "a+",          "a+",            "00100", "" },
	{ "^a$",         "^a$",           "11000", "" },
	{ "b$",          "b$",            "01000", "" },
	{ "[a-dqp-t]+",  "[a-dqp-t]+",    "00100", "[abcdqprst]+" },
	{ "a*b[ch]",     "a*b[ch]",       "00000", "" },
	{ "^a*b[ch]",    "^a*b[ch]",      "10000", "" },
	{ "^a+b[ch]",    "^a+b[ch]",      "10000", "" },
	{ "a+b[ch]",     "a+b[ch]",       "00000", "" },
	{ "a+$",         "a+$",           "01100", "" },
	{ "hello",       "hello",         "00000", "" },
	{ "a?b",         "a?b",           "00000", "" },
	{ "ab?",         "ab?",           "00100", "" },
	{ "[a-f]?g",     "[a-f]?g",       "00000", "[abcdef]?g" },
	{ ".?[bc]",      ".?[bc]",        "00000", "" },
	{ "x.*.*y",      "x.*y",          "00000", "" },
	{ "a+.*.*b[ch]", "a+.*b[ch]",     "00000", "" },
	{ "a+.+.+b[ch]", "a+..+b[ch]",    "00000", "" },
	{ ".*hello",     "hello",         "00000", "" },
	{ ".+hello",     ".hello",        "00000", "" },
	{ "hello.*",     "hello",         "00000", "" },
	{ "hello.+",     "hello.",        "00000", "" },
};


static void goodtests( void )
{
	struct REFlags reflags;

	int nel = NELEMS(oktests);
	for( int i=0; i < nel; i++ )
	{
		printf( "test: %s -> ", oktests[i].str );
		init_REFlags( &reflags );

		char128 origstr;
		strcpy( origstr, oktests[i].str );
		simplify_regex( &reflags, oktests[i].str );
		printf( "modified str %s\n", oktests[i].str );

		char256 label;
		sprintf( label, "simpl(%s).answer", origstr );
		teststring( oktests[i].str, oktests[i].answer, label );

		RE re = parse_regex( &reflags, oktests[i].str );
		printf( "parsed RE %s: ", oktests[i].str );
		print_RE( stdout, re );
		putchar( '\n' );

		char128 flagstr;
		flag2str( &reflags, flagstr );
		sprintf( label, "simpl(%s).flags", origstr );
		teststring( flagstr, oktests[i].flagstr, label );

		printf( " regenerating src: " );
		char256 as_src;
		stringify_RE( as_src, re );
		printf( "%s\n", as_src );

		sprintf( label, "simpl(%s).stringify", origstr );
		// correct answer is regen or answer
		if( oktests[i].regen[0] == '\0' )
		{
			strcpy( oktests[i].regen, oktests[i].answer );
		}
		teststring( as_src, oktests[i].regen, label );

		char *error = check_regex( re );
		if( error == NULL )
		{
			printf( "checks: fine\n" );
			sprintf( label, "simpl(%s).error(null)", origstr );
			testcond( true, label );
		} else
		{
			printf( "checks: error: '%s'\n", error );
			sprintf( label, "simpl(%s).error(%s)",
				origstr, error );
			testcond( false, label );
			free( error );
		}

		free_RE( re );
	}
}


// ---- error tests (i.e. ones that should generate an error) ----


typedef struct {
	char128 str;
	char128 error;
} testerror;


static testerror errtests[] =
{
	{ "a+a",               "a+a has char 'a' in common in adjacent a+a" },
	{ "a*a",               "a*a has char 'a' in common in adjacent a*a" },
	{ "[ab]*a",            "[ab]*a has char 'a' in common in adjacent [ab]*a" },
	{ "[ab]*[bcd]",        "[ab]*[bcd] has char 'b' in common in adjacent [ab]*[bcd]" },
	{ "he[def]+[dunc]*th", "he[def]+[dunc]*th has char 'd' in common in adjacent [def]+[dunc]*" },
};


static void badtests( void )
{
	struct REFlags reflags;
	char256 label;

	int nel = NELEMS(errtests);
	for( int i=0; i < nel; i++ )
	{
		printf( "etest: %s ->\n", errtests[i].str );
		init_REFlags( &reflags );

		char128 origstr;
		strcpy( origstr, errtests[i].str );
		simplify_regex( &reflags, errtests[i].str );
		//printf( "modified str %s\n", errtests[i].str );

		RE re = parse_regex( &reflags, errtests[i].str );
		//printf( "parsed RE %s: ", errtests[i].str );
		//print_RE( stdout, re );
		//putchar( '\n' );

		char *error = check_regex( re );
		if( error == NULL )
		{
			sprintf( label, "err:simpl(%s).nullerror",
				origstr );
			teststring( "", errtests[i].error, label );
		} else
		{
			sprintf( label, "err:simpl(%s).error",
				origstr );
			teststring( error, errtests[i].error, label );
			free( error );
		}

		free_RE( re );
	}
}


int main( void )
{
	goodtests();
	badtests();

	return 0;
}
