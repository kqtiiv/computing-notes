// testageinfo.c: unit test for ageinfo (relies on dlist working too)

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
#include "ageinfo.h"


// ------------- sum up member ages per dept ----------------


// ageinfo a; calc_ageinfo( dl, &a );
// double exp_avg[] = { FIRSTDEPTAVGAGE, SECONDDEPTAVGAGE.. };
// int exp_nmbrs[] =  { FIRSTDEPTNMEMBERS, SECONDDEPTNMEMBERS... };
// test_avgage_dept( &a, exp_ndepts, exp_nmbrs, exp_avg );
//	Test that the average ages and #members in a match exp_ndepts,
//	exp_nmbrs[ndepts] and exp_avg[ndepts]
//
static void test_avgage_dept( ageinfo *a, int exp_ndepts, int *exp_nmbrs, double *exp_avg )
{
  //testint( a->ndepts, exp_ndepts, "#depts" );

  for( int i=0; i<exp_ndepts; i++ )
  {
  	char label[64];
	int sumage = a->agesum[i];
	int nmbrs  = a->nmembers[i];
	sprintf( label, "#nmbrs(dept %s)", a->deptname[i] );
	testint( nmbrs, exp_nmbrs[i], label );
	assert( nmbrs > 0 );
	double avg = a->avgage[i];
	printf( "debug: dept#%d, name %s, sumage %d, #mbrs %d, avg %g\n",
		i, a->deptname[i], sumage, nmbrs, avg );
	sprintf( label, "avgage(dept %s)", a->deptname[i] );
  	testdouble( avg, exp_avg[i], label );
  }
}


#define LDK_AGE  50
#define DCW_AGE  58
#define IVAN_AGE 25


int main( void ) {
  list dl = make_dlist();

  department csg = make_department( "CSG" );
  //printf( "csg() is " ); department_print( stdout, csg ); putchar('\n');

  add_to_department( csg, make_person( "Ms", "Shirley", "Kamara", 50 ) );
  //printf( "csg(ldk) is " ); department_print( stdout, csg ); putchar('\n');

  add_department_to_dlist( dl, csg );

  department found = find_or_add_department( dl, "EdTech" );
  assert( found != NULL );

  add_to_department( csg, make_person( "Mr", "Bob", "White", DCW_AGE ) );
  //printf( "csg(ldk,dcw) is " ); department_print( stdout, csg ); putchar('\n');

  //printf( "final csg dept is " ); department_print( stdout, csg ); putchar('\n');

  //printf( "dl is currently:\n" ); print_dl( dl );

  department edtech = find_department( dl, "EdTech" );
  //printf( "edtech() is " ); department_print( stdout, edtech ); putchar('\n');

  add_to_department( edtech, make_person( "Mr", "Hilary", "Procaccini", IVAN_AGE ) );
  //printf( "edtech(ivan) is " ); department_print( stdout, edtech ); putchar('\n');

  ageinfo a;
  calc_ageinfo( dl, &a );
  int ndepts = a.ndepts;
  testint( ndepts, 2, "#depts" );

  int expectednmbrs[]  = { 2, 1 };
  double expectedavg[] = { (DCW_AGE+LDK_AGE)/2, IVAN_AGE };
  test_avgage_dept( &a, ndepts, expectednmbrs, expectedavg );
  free_ageinfo( &a );

  list_free( dl );

  return 0;
}
