// ageinfo: calculate age info of each dept

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "person.h"
#include "list.h"
#include "department.h"
#include "dlist.h"
#include "ageinfo.h"


// ageinfo a;
// process_dm( deptname, member, &a );
//	Process (deptname,member) pair, updating the ageinfo record:
//	Sum up the ages of the current department, count the #members of
//	the current dept. When the deptname changes, move onto the next dept.
//
static void process_dm( char *deptname, person member, void *state ) {
  ageinfo *a = state;

  printf( "debug: process_dm(): dept %s, member ", deptname );
  person_print( stdout, member );
  putchar( '\n' );

  // Task 3: WRITE YOUR CODE HERE
  if (a->currdept == -1) 
  {
    a->currdept = 0;
    strcpy(a->deptname[0], deptname);
    a->agesum[0] = 0;
    a->nmembers[0] = 0;
  } 
  else if (strcmp(deptname, a->deptname[a->currdept]) != 0)
  {
    a->currdept++;
    strcpy(a->deptname[a->currdept], deptname);
    a->agesum[a->currdept] = 0;
    a->nmembers[a->currdept] = 0;
  }

  a->nmembers[a->currdept]++;
  a->agesum[a->currdept] += member->age;
}


// ageinfo a;
// calc_ageinfo( dl, &a );
//   Calculate the sum of member ages and number of members
//   for each dept in dl, storing them in a, then compute the
//   per-dept average ages (also storing them in a).
//
void calc_ageinfo( list dl, ageinfo *a )
{
  // Initialise *a
  a->ndepts    = list_len( dl );
  a->currdept  = -1;
  a->deptname  = malloc( a->ndepts * sizeof(deptnamestr) );
  assert( a->deptname != NULL );
  a->agesum    = malloc( a->ndepts * sizeof(int) );
  assert( a->agesum != NULL );
  a->nmembers  = malloc( a->ndepts * sizeof(int) );
  assert( a->nmembers != NULL );
  a->avgage    = malloc( a->ndepts * sizeof(double) );
  assert( a->nmembers != NULL );

  dlist_foreach_member( dl, &process_dm, a );

  assert( a->currdept == a->ndepts-1 );

  // now calculate the average ages:
  for( int i=0; i<a->ndepts; i++ ) {
    int nmbrs    = a->nmembers[i];
    assert( nmbrs > 0 );
    int sum      = a->agesum[i];
    double avg   = (double)sum / (double)nmbrs;
    a->avgage[i] = avg;
    printf( "dept %s: #mbrs %d, sum ages %d, avg age %g\n",
        a->deptname[i], nmbrs, sum, avg );
  }
}


void free_ageinfo( ageinfo *a )
{
  free( a->deptname );
  free( a->agesum );
  free( a->nmembers );
  free( a->avgage );
}
