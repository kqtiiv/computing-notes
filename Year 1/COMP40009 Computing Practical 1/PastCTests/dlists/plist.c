// plist.c: convenience module to make creating linked lists of people easier

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "person.h"
#include "list.h"
#include "plist.h"


// p_sprint( buf, el );
//	wrapper for person_sprint
//
static void p_sprint( char *buf, LIST_ELEMENT el ) {
  person_sprint( buf, (person)el );
}

// p_free( el );
//	wrapper for person_free
//
static void p_free( LIST_ELEMENT el ) {
  person_free( (person)el );
}


// list l = make_plist();
//	Make an empty linked list of people (person)
//
list make_plist( void )
{
  list pl = make_list( &person_compare, &p_sprint, &p_free );
  return pl;
}
