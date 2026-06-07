// testperson.c: unit test for person

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "testutils.h"

#include "person.h"


int main( void ) {
  person p = make_person( "Mr", "Lloyd", "Kamara", 48 );
  printf( "p(ldk) is " ); person_print( stdout, p ); putchar('\n');
  teststring( p->firstname, "Lloyd", "p(ldk).firstname" );
  teststring( p->lastname, "Kamara", "p(ldk).lastname" );
  teststring( p->title, "Mr", "p(ldk).title" );
  testint( p->age, 48, "p(ldk).age" );

  person p2 = make_person( "Mr", "Duncan", "White", 57 );
  printf( "p2(dcw) is " ); person_print( stdout, p2 ); putchar('\n');

  teststring( p2->firstname, "Duncan", "p2(dcw).firstname" );
  teststring( p2->lastname, "White", "p2(dcw).lastname" );

  person_uppercase_names( p2 );
  teststring( p2->firstname, "DUNCAN", "uppercase(p2(dcw).firstname" );
  teststring( p2->lastname, "WHITE", "uppercase(p2(dcw)).lastname" );

  person p3 = make_person( "Mr", "Will", "Knottenbelt", 35 );
  printf( "p3(will) is " ); person_print( stdout, p3 ); putchar( '\n' );

  teststring( p3->firstname, "Will", "p3(will).firstname" );

  teststring( p3->lastname, "Knottenbelt", "p3(will).lastname" );

  person_free( p );
  person_free( p2 );
  person_free( p3 );

  return 0;
}
