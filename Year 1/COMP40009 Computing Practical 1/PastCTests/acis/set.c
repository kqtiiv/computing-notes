// set.c:	a simple set of strings, to which items are only ever added

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "set.h"


struct set
{
	int cap;	// current capacity, i.e. allocated size of data
	int n;		// current number of entries in data (n<=cap)
	char **data;	// actual data, growable.  pointers into buf
	int bufcap;	// the allocated size of buf
	int buflen;	// the current amount of data stored in buf
	char *buf;	// the buffer of underlying data
};


// A set is a growable dynarray of string pointers into a common
// string buffer (storage for which is also maintained inside the set).


static int cmpstringp( const void *p1, const void *p2 )
{
	// The actual arguments to this function are "pointers to
	// pointers to char", but strcmp(3) arguments are "pointers
	// to char", hence the following:
	char **a = (char **)p1;
	char **b = (char **)p2;
	//printf( "debug: cmpstringp: a=%s, b=%s\n", *a, *b );
	return strcmp( *a, *b );
}


// set s = make_set()
//	Create an empty set.
//
set make_set( void )
{
	set s = malloc( sizeof(struct set) );
	assert( s != NULL );
	s->cap  = 10;
	s->n    = 0;
	s->data = malloc( s->cap * sizeof(char *) );
	assert( s->data != NULL );
	s->bufcap = 10;
	s->buflen = 0;
	s->buf = malloc( s->bufcap * sizeof(char) );
	assert( s->buf != NULL );
	return s;
}


// int nocc = countch( char *str, char ch );
//	Count how many occurrences of <ch> that are in <str>
//
static int countch( char *str, char ch )
{
	int n = 0;
	for( char *p=str; *p!='\0'; p++ )
	{
		if( *p==ch ) n++;
	}
	return n;
}


// set s = make_set_from_string( char *data, char sep )
//	Create a set by parsing a string where the set members
//	are separated by <sep>
//
set make_set_from_string( char *data, char sep )
{
	set s = malloc( sizeof(struct set) );
	assert( s != NULL );
	s->bufcap  = 1 + strlen(data);
	s->buflen  = s->bufcap;
	s->buf     = strdup( data );
	assert( s->buf != NULL );

	int n = 1 + countch( data, sep );
	s->cap = n;
	s->n   = n;
	s->data = malloc( n * sizeof(char *) );
	assert( s->data != NULL );
	char *cp = s->buf;
	for( int i=0; i<n; i++ )
	{
		s->data[i] = cp;
		cp = strchr( cp, sep );
		if( cp != NULL ) *cp++ = '\0';
	}

	// sort data
	qsort( s->data, s->n, sizeof(char *), &cmpstringp );

	return s;
}


// set news = clone_set( set s);
//	Clone a set entirely
//
set clone_set( set s )
{
	set new = malloc( sizeof(struct set) );
	assert( new != NULL );
	new->bufcap = s->buflen;
	new->buflen = s->buflen;
	new->buf    = malloc( new->buflen * sizeof(char) );
	assert( new->buf != NULL );
	memcpy( new->buf, s->buf, new->buflen );
	assert( new->buf != NULL );

	new->cap = s->cap;
	new->n   = s->n;
	new->data = malloc( new->cap * sizeof(char *) );
	assert( new->data != NULL );
	for( int i=0; i<new->n; i++ )
	{
		new->data[i] = new->buf + (s->data[i]-s->buf);
	}
	for( int i=new->n; i<new->cap; i++ )
	{
		new->data[i] = NULL;
	}
	return new;
}


// void add_set( s, item );
//	Add a given item to set s, growing it if necessary
//
void add_set( set s, char *item )
{
	bool isin = in_set( s, item );
	if( isin ) return;		// already there?

	//printf( "debug: add set.  adding item %s to set\n", item );

	int len = strlen(item)+1;
	if( s->buflen + len > s->bufcap )
	{
		s->bufcap += 100 + len;
		char *old = s->buf;
		char *new = realloc( old, s->bufcap * sizeof(char) );
		if( old != new )
		{
			s->buf = new;
			// the buffer has moved, so we'll need
			// to alter all the data pointers
			for( int i=0; i<s->n; i++ )
			{
				s->data[i] = new+(s->data[i]-old);
			}
		}
		assert( s->buf != NULL );
	}
	char *sp = s->buf + s->buflen;
	strcpy( sp, item );
	s->buflen += len;

	if( s->n >= s->cap )
	{
		s->cap += 100;
		assert( s->n <= s->cap );
		s->data = realloc( s->data, s->cap * sizeof(char *) );
		assert( s->data != NULL );
	}
	s->data[s->n++] = sp;

	#if 0
	printf( "debug: BEFORE qsort, set ptrs are: " );
	for( int i=0; i<s->n; i++ )
	{
		printf( "%s,", s->data[i] );
	}
	putchar( '\n' );
	#endif

	// re-sort data (could use insertion sort if we cared enough)
	qsort( s->data, s->n, sizeof(char *), &cmpstringp );

	#if 0
	printf( "debug: AFTER qsort, set ptrs are: " );
	for( int i=0; i<s->n; i++ )
	{
		printf( "%s,", s->data[i] );
	}
	putchar( '\n' );
	#endif

	#if 0
	printf( "debug: after appending %s, set is ", item );
	print_set( s, stdout ); putchar( '\n' );
	char *beyond = s->buf + s->buflen;
	printf( "debug: set buffer is: " );
	for( char *p = s->buf; p<beyond; p++ )
	{
		putchar( *p=='\0' ? 'X' : *p );
	}
	putchar( '\n' );
	printf( "debug: set ptrs are: " );
	for( int i=0; i<s->n; i++ )
	{
		printf( "%s,", s->data[i] );
	}
	putchar( '\n' );
	#endif
}


// bool isin = in_set( s, value );
//	Given a set s, return true iff value is "in" s.
//	Let's use bsearch()...
//
bool in_set( set s, char *value )
{
	char **res = bsearch( &value, s->data, s->n, sizeof(char *),
			      &cmpstringp );
	return res != NULL;

	#if 0
	for( int i=0; i<s->n; i++ )
	{
		if( strcmp( value, s->data[i] ) == 0 ) return true;
	}
	return false;
	#endif
}


// foreach_set( s, smf, stateptr );
//   iterator function: call smf( element, stateptr )
//   foreach element in s.
//
void foreach_set( set s, set_member_func smf, void * stateptr )
{
	assert( smf != NULL );
	for( int i=0; i<s->n; i++ ) {
		(*smf)( s->data[i], stateptr );
	}
}


// print_set( s, out );
//	Print set s to open writable file out
//	as a CSV list.
//
void print_set( set s, FILE *out )
{
	for( int i=0; i<s->n; i++ )
	{
		if( i>0 ) putc( ',', out );
		printf( "%s", s->data[i] );
	}
}


// free_set( s );
//	Free set s
//
void free_set( set s )
{
	free( s->data );
	free( s->buf );
	free( s );
}


static void add_one( char *member, void *disguised_set )
{
	add_set( (set)disguised_set, member );
}


// set u = union_set( s1, s2 );
//	Build a new set of all members in either
//	s1 or s2 or both
//
set union_set( set s1, set s2 )
{
	set uset = make_set();
	foreach_set( s1, add_one, uset );
	foreach_set( s2, add_one, uset );
	return uset;
}
