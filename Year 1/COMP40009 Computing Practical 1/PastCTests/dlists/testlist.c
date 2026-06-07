// testlist.c: tiny example of using generic linked lists - of strings

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "testutils.h"
#include "list.h"


// support functions to allow a list of strings..

static void mystr_sprint( char *buf, LIST_ELEMENT el ) {
  strcpy( buf, (char *)el );
}

static int mystr_compare( const LIST_ELEMENT ap, const LIST_ELEMENT bp ) {
  char *a = (char *)ap;
  char *b = (char *)bp;
  return strcmp( a, b );
}

// sumlencb( head, stateptr );
//   head is a disguised string, stateptr is a disguised int *total,
//   add strlen(head) to the total.
//
static void sumlencb( LIST_ELEMENT head, void *stateptr ) {
  char *s = head;
  int *total = stateptr;
  *total += strlen( s );
}

#define DUNC  "duncan"
#define WILL  "will"
#define LLOYD "lloyd"
#define ADAM  "adam"

// strlist_tests();
//   Perform a large number of "list of readonly string" tests
//
static void strlist_tests( void )
{
  list l = make_list( &mystr_compare, &mystr_sprint, NULL );

  printf( "sl() is " ); list_print( stdout, l ); putchar('\n');
  int n = list_len( l );
  testint( n, 0, "sl().len" );

  list_add( l, DUNC );
  printf( "sl(dunc) is " ); list_print( stdout, l ); putchar('\n');
  n = list_len( l );
  testint( n, 1, "sl(dunc).len" );

  int total = 0;
  list_foreach( l, &sumlencb, &total );
  printf( "totalnamelen=%d\n", total );
  testint( total, 6, "sl(dunc).totalnamelen" );

  char *name = list_nth( 0, l );
  teststring( name, DUNC, "sl(dunc).0" );

  list_addsorted( l, WILL );
  fputs( "sl(dunc,will): prints as ", stdout );
  list_print( stdout, l );
  putchar( '\n' );
  n = list_len( l );
  testint( n, 2, "sl(dunc,will).len" );

  total = 0;
  list_foreach( l, &sumlencb, &total );
  printf( "totallen=%d\n", total );
  testint( total, 10, "sl(dunc,will).totallen" );

  name = list_nth( 0, l );
  teststring( name, DUNC, "sl(dunc,will).0" );

  name = list_nth( 1, l );
  teststring( name, WILL, "sl(dunc,will).1" );

  list_addsorted( l, LLOYD );
  fputs( "sl(dunc,will,lloyd): prints as ", stdout );
  list_print( stdout, l );
  putchar( '\n' );
  n = list_len( l );
  testint( n, 3, "sl(dunc,will,lloyd).len" );

  total = 0;
  list_foreach( l, &sumlencb, &total );
  printf( "totallen=%d\n", total );
  testint( total, 15, "sl(dunc,will,lloyd).totalnamelen" );

  name = list_nth( 0, l );
  teststring( name, DUNC, "sl(dunc,will,lloyd).0" );

  name = list_nth( 1, l );
  teststring( name, LLOYD, "sl(dunc,will,lloyd).1" );

  name = list_nth( 2, l );
  teststring( name, WILL, "sl(dunc,will,lloyd).2" );

  list_addsorted( l, ADAM );
  fputs( "sl(dunc,will,lloyd,adam): prints as ", stdout );
  list_print( stdout, l );
  putchar( '\n' );
  n = list_len( l );
  testint( n, 4, "sl(dunc,will,lloyd,adam).len" );

  total = 0;
  list_foreach( l, &sumlencb, &total );
  printf( "totallen=%d\n", total );
  testint( total, 19, "sl(dunc,will,lloyd,adam).totalnamelen" );

  name = list_nth( 0, l );
  teststring( name, ADAM, "sl(dunc,will,lloyd).0" );

  name = list_nth( 1, l );
  teststring( name, DUNC, "sl(dunc,will,lloyd).1" );

  name = list_nth( 2, l );
  teststring( name, LLOYD, "sl(dunc,will,lloyd,adam).2" );

  name = list_nth( 3, l );
  teststring( name, WILL, "sl(dunc,will,lloyd,adam).3" );

  list_free( l );
}


int main( void ) {
  // a set of tests with a linked list of readonly strings
  strlist_tests();

  return 0;
}
