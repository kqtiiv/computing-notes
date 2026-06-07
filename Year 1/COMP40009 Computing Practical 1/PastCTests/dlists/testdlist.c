// testdlist.c: unit test for dlist

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
#include "dlist.h"


// ------------- print (deptname,member) pairs ----------------

// prdm( deptname, member, &ndepts );
//	Print each (deptname,member) pair, incrementing ndepts as we do
//
static void prdm( char *deptname, person member, void *state ) {
  int *np = state;
  printf( "%d: dept %s, member ", (*np)++, deptname );
  person_print( stdout, member );
  putchar( '\n' );
}


// print_dl( dl );
//	print the given dept list dl
//
static void print_dl( list dl )
{
  int deptno = 1;
  dlist_foreach_member( dl, &prdm, &deptno );
  //putchar( '\n' );
}


// print_dept( d );
//	print the given department
//
static void print_dept( department d )
{
    int deptno = 1;
    department_foreach_member( d, &prdm, &deptno );
    putchar( '\n' );
}


// ------------- sum up member ages ----------------

// sumdm( deptname, member, &sumages );
//	Add the member's age to sumages.
//
static void sumdm( char *deptname, person member, void *state ) {
  int *s = state;
  *s += member->age;
  // *((int *)s) += member->age;
}


// test_age_sum( dl, expectedsum );
//	Sum up all the ages of all the people in all depts
//	in sl, test that it is expectedsum
//
static void test_age_sum( list dl, int expectedsum )
{
    int s = 0;
    dlist_foreach_member( dl, &sumdm, &s );
    printf( "debug: found age sum = %d\n", s );
    testint( s, expectedsum, "sum(ages)" );
}


#define LDK_AGE  50
#define DCW_AGE  58
#define IVAN_AGE 25


int main( void ) {
  list dl = make_dlist();
  printf( "dl() is " ); dlist_print( stdout, dl );
  int n = list_len( dl );
  testint( n, 0, "dl().len" );

  department csg = make_department( "CSG" );
  printf( "csg() is " ); department_print( stdout, csg ); putchar('\n');
  teststring( csg->name, "CSG", "csg().name" );

  list cml = csg->members;
  n = list_len( cml );
  testint( n, 0, "cml().len" );

  add_to_department( csg, make_person( "Mr", "Shirley", "Kamara", 50 ) );
  printf( "csg(ldk) is " ); department_print( stdout, csg ); putchar('\n');
  teststring( csg->name, "CSG", "csg(ldk).name" );
  n = list_len( cml );
  testint( n, 1, "cml(ldk).len" );

  person p = list_nth( 0, cml );

  teststring( p->firstname, "Shirley", "csg(ldk).firstname" );
  teststring( p->lastname, "Kamara", "csg(ldk).lastname" );
  teststring( p->title, "Mr", "csg(ldk).title" );
  testint(    p->age, LDK_AGE, "csg(ldk).age" );

  add_department_to_dlist( dl, csg );

  department found = find_department( dl, "CSG" );
  testcond(  found != NULL, "find(csg)" );
  if( found != NULL )
  {
    teststring( found->name, "CSG", "csg(ldk).find('CSG').name" );
    printf( "csg(ldk).find('CSG').csg members: " ); print_dept( found );
  }

  found = find_department( dl, "nosuch" );
  testcond( found == NULL, "!find(nosuch)" );
  if( found != NULL )
  {
    printf( "(impossible) nosuch name: %s\n", found->name );
    printf( "(impossible) nosuch members: " ); print_dept( found );
  }

  found = find_or_add_department( dl, "EdTech" );
  testcond( found != NULL, "find(-EdTech) has added EdTech" );
  if( found != NULL )
  {
    printf( "EdTech name: %s\n", found->name );
    teststring( found->name, "EdTech", "find(-EdTech).find('EdTech').name" );
    printf( "EdTech dept: " ); print_dept( found );

    list etl = found->members;
    n = list_len( etl );
    testint( n, 0, "etl(edtech).len" );
  }

  add_to_department( csg, make_person( "Mr", "Bob", "White", DCW_AGE ) );
  printf( "csg(ldk,dcw) is " ); department_print( stdout, csg ); putchar('\n');

  n = list_len( cml );
  testint( n, 2, "cml(ldk,dcw).len" );

  p = list_nth( 0, cml );

  teststring( p->firstname, "Shirley", "csg(ldk,dcw).it1.firstname" );
  teststring( p->lastname, "Kamara", "csg(ldk,dcw).it1.lastname" );
  teststring( p->title, "Mr", "csg(ldk,dcw).it1.title" );
  testint(    p->age, LDK_AGE, "csg(ldk,dcw).it1.age" );

  p = list_nth( 1, cml );
  teststring( p->firstname, "Bob", "csg(ldk,dcw).it2.firstname" );
  teststring( p->lastname, "White", "csg(ldk,dcw).it2.lastname" );

  printf( "final csg dept is " ); department_print( stdout, csg ); putchar('\n');

  printf( "dl is currently:\n" ); print_dl( dl );

  department edtech = find_department( dl, "EdTech" );
  printf( "edtech() is " ); department_print( stdout, edtech ); putchar('\n');

  add_to_department( edtech, make_person( "Mr", "Hilary", "Procaccini", IVAN_AGE ) );
  printf( "edtech(ivan) is " ); department_print( stdout, edtech ); putchar('\n');
  teststring( edtech->name, "EdTech", "edtech(ivan).name" );

  list etl = edtech->members;
  n = list_len( etl );
  testint( n, 1, "edtech(ivan).len" );

  p = list_nth( 0, etl );
  teststring( p->firstname, "Hilary", "edtech(ivan).firstname" );
  teststring( p->lastname, "Procaccini", "edtech(ivan).lastname" );

  printf( "final dl is:\n" ); print_dl( dl );

  test_age_sum( dl, DCW_AGE+LDK_AGE+IVAN_AGE );

  int ndepts = list_len( dl );
  testint( ndepts, 2, "len(dl)" );

  list_free( dl );

  return 0;
}
