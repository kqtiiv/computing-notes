// person.c: a simple person ADT - implementation

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "person.h"


// person p = make_person( title, firstname, lastname, age );
//   Constructor: create and return a new person with the given values.
//
person make_person( char *title, char *first, char *last, int age ) {
  person p = malloc( sizeof(struct person) );
  assert( p != NULL );

  COPYSTR( p->title, title, TITLELEN );
  COPYSTR( p->firstname, first, NAMELEN );
  COPYSTR( p->lastname, last, NAMELEN );
  p->age = age;

  return p;
}


// person_sprint( buf, p );
//   String-print person p into preallocated buffer buf,
//   assuming that the buffer is big enough
//
void person_sprint( char *buf, person p ) {
  sprintf( buf, "%s %s %s (age %d)", p->title, p->firstname, p->lastname, p->age );
}


// person_print( out, p );
//   Print person p to file out
//
void person_print( FILE *out, person p ) {
  char buf[100];
  person_sprint( buf, p );
  fprintf( out, "%s", buf );
}


// person_free( p );
//   Free the given person
//
void person_free( person p ) {
  free( p );
}


static void uppercase_one_string( char *str ) 
{
  for (int i=0; i<strlen(str); i++) {
    str[i] = toupper(str[i]);
  }
}

// person_uppercase_names( p );
//   Uppercase both the person's firstname and lastname.
//
void person_uppercase_names( person p ) {
  // Task 1: WRITE YOUR OWN CODE HERE
  uppercase_one_string(p->firstname);
  uppercase_one_string(p->lastname);
}


// int result = person_compare( p1, p2 );
//   qsort() compatible comparison function, comparing p1 and p2,
//   first by lastname, then by firstname and finally by age.
//
int person_compare( const void *a, const void *b ) {
   person ap = (person)a;
   person bp = (person)b;
   int cmp = strcmp( ap->lastname, bp->lastname );
   if( cmp != 0 ) return cmp;
   cmp = strcmp( ap->firstname, bp->firstname );
   if( cmp != 0 ) return cmp;
   return ap->age - bp->age;
}
