// testlist.c: unit test of list of people

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "testutils.h"

#include "list.h"
#include "person.h"
#include "plist.h"


static void puppercasecb( LIST_ELEMENT head, void *stateptr ) {
  person_uppercase_names( (person)head );
}

static void sumagecb( LIST_ELEMENT head, void *stateptr ) {
  person p = head;
  int *total = stateptr;
  *total += p->age;
}


static void describe( list l ) {
  int total = 0;
  list_foreach( l, &sumagecb, &total );
  int len = list_len( l );
  printf( ", len=%d, agesum=%d\n", len, total );
}


int main( void ) {
  list l = make_plist();
  printf( "ll() is " ); list_print( stdout, l );
  describe( l );
  int n = list_len( l );
  testint( n, 0, "ll().len" );

  list_addsorted( l, make_person( "Mr", "Lloyd", "Kamara", 48 ) );
  printf( "ll(ldk) is " ); list_print( stdout, l );
  describe( l );
  n = list_len( l );
  testint( n, 1, "ll(ldk).len" );

  person p = list_nth( 0, l );
  teststring( p->firstname, "Lloyd", "ll(ldk).it0.firstname" );

  list_addsorted( l, make_person( "Mr", "Duncan", "White", 45 ) );
  printf( "ll(ldk,dcw) is " ); list_print( stdout, l );
  describe( l );
  n = list_len( l );
  testint( n, 2, "ll(ldk,dcw).len" );

  int total = 0;
  list_foreach( l, &sumagecb, &total );
  printf( "totalage=%d\n", total );
  testint( total, 93, "ll(ldk,dcw).totalage" );

  p = list_nth( 0, l );
  teststring( p->firstname, "Lloyd", "ll(ldk,dcw).it0.firstname" );
  p = list_nth( 1, l );
  teststring( p->firstname, "Duncan", "ll(ldk,dcw).it1.firstname" );

  list_addsorted( l, make_person("Mr", "Will", "Knottenbelt", 35) );
  fputs( "ll(ldk,will,dcw): prints as ", stdout );
  list_print( stdout, l );
  putchar( '\n' );
  n = list_len( l );
  testint( n, 3, "ll(ldk,will,dcw).len" );

  p = list_nth( 0, l );
  teststring( p->firstname, "Lloyd", "ll(ldk,will,dcw).it0.firstname" );
  p = list_nth( 1, l );
  teststring( p->firstname, "Will", "ll(ldk,will,dcw).it1.firstname" );
  p = list_nth( 2, l );
  teststring( p->firstname, "Duncan", "ll(ldk,will,dcw).it2.firstname" );

  total = 0;
  list_foreach( l, &sumagecb, &total );
  printf( "totalage=%d\n", total );
  testint( total, 128, "ll(ldk,will,dcw).totalage" );

  list_foreach( l, &puppercasecb, NULL );

  p = list_nth( 0, l );
  teststring( p->lastname, "KAMARA", "ll(ldk,will,dcw).it0.firstname" );
  p = list_nth( 1, l );
  teststring( p->lastname, "KNOTTENBELT", "ll(ldk,will,dcw).it1.firstname" );
  p = list_nth( 2, l );
  teststring( p->lastname, "WHITE", "ll(ldk,will,dcw).it2.firstname" );

  list_free( l );

  return 0;
}
