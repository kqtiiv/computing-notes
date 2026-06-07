// testdepartment.c: unit test for department

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "testutils.h"

#include "person.h"
#include "list.h"
#include "department.h"


static void eachcb( char *deptname, person member, void *state ) {
  printf( "dept %s, member ", deptname );
  person_print( stdout, member );
  char *cs = state;
  printf( ", state %s\n", cs==NULL?"NULL":cs );
}

int main( void ) {
  department csg = make_department( "CSG" );
  printf( "csg() is " ); department_print( stdout, csg ); putchar('\n');
  teststring( csg->name, "CSG", "csg().name" );

  list cml = csg->members;
  int n = list_len( cml );
  testint( n, 0, "csg().len" );

  add_to_department( csg, make_person( "Mr", "Lloyd", "Kamara", 18 ) );
  printf( "csg(ldk) is " ); department_print( stdout, csg ); putchar('\n');
  n = list_len( cml );
  testint( n, 1, "csg(ldk).len" );

  person p = list_nth( 0, cml );

  teststring( p->firstname, "Lloyd", "csg(ldk).it1.firstname" );
  teststring( p->lastname, "Kamara", "csg(ldk).it1.lastname" );
  teststring( p->title, "Mr", "csg(ldk).it1.title" );
  testint(    p->age, 18, "csg(ldk).age" );


  add_to_department( csg, make_person( "Mr", "Duncan", "White", 20 ) );
  printf( "csg(ldk,dcw) is " ); department_print( stdout, csg ); putchar('\n');

  p = list_nth( 0, cml );

  teststring( p->firstname, "Lloyd", "csg(ldk,dcw).it1.firstname" );
  teststring( p->lastname, "Kamara", "csg(ldk,dcw).it1.lastname" );
  teststring( p->title, "Mr", "csg(ldk,dcw).it1.title" );
  testint(    p->age, 18, "csg(ldk,dcw).age" );

  p = list_nth( 1, cml );
  teststring( p->firstname, "Duncan", "csg(ldk,dcw).it2.firstname" );
  teststring( p->lastname, "White", "csg(ldk,dcw).it2.lastname" );
  teststring( p->title, "Mr", "csg(ldk,dcw).it2.title" );
  testint(    p->age, 20, "csg(ldk,dcw).it2.age" );

  department_foreach_member( csg, &eachcb, NULL );


  department edtech = make_department( "EdTech" );
  printf( "edtech() is " ); department_print( stdout, edtech ); putchar('\n');

  teststring( edtech->name, "EdTech", "edtech().name" );

  list etl = edtech->members;
  n = list_len( etl );
  testint( n, 0, "edtech().len" );

  add_to_department( edtech, make_person( "Mr", "Ivan", "Procaccini", 25 ) );
  printf( "edtech(ivan) is " ); department_print( stdout, edtech ); putchar('\n');

  n = list_len( etl );
  testint( n, 1, "edtech(ivan).len" );
  p = list_nth( 0, etl );
  teststring( p->firstname, "Ivan", "edtech(ivan).0.firstname" );
  teststring( p->lastname, "Procaccini", "edtech(ivan).0.lastname" );

  department_foreach_member( csg, &eachcb, "one" );
  department_foreach_member( edtech, &eachcb, "two" );

  department_free( edtech );
  department_free( csg );

  return 0;
}
