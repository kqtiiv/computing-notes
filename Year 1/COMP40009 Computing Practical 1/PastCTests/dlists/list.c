//
// list.c: singly linked list-of-generic(void *) pointers - implementation
//

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "list.h"


// the data: the actual internal singly linked list
typedef struct listdata *listdata;
struct listdata {
  LIST_ELEMENT  head;
  listdata   	tail;
};

// our ADT comprises the data and various element operators.
struct list {
  listdata     data;	// the data - the linked list itself
  list_cmpf    cmpf;	// the element comparator function
  list_sprintf spef;	// the element sprint function
  list_freef   fef;	// the element print function
};


// list l = make_list( &compareelementf, &sprintelementf, &freelementf );
//   Create an empty list with the given element comparator,
//   sprint element and free element function pointers.
//
list make_list( list_cmpf cmpf, list_sprintf spef, list_freef fef )
{
  // Task 2a: WRITE YOUR OWN CODE HERE
  list l = malloc(sizeof(*l));
  assert(l != NULL);

  l->data = NULL;

  l->cmpf = cmpf;	
  l->spef = spef;
  l->fef = fef;

  return l;
}


// listdata l = listdata_cons( head, tail );
//   Add a list element head to the front of a listdata tail.
//   Return the new listdata.
//
static listdata listdata_cons( LIST_ELEMENT head, listdata tail ) {
  // Task 2b: WRITE YOUR OWN CODE HERE
  listdata l = malloc(sizeof(*l));
  assert(l != NULL);

  l->head = head;
  l->tail = tail;

  return l;
}


// list_add( l, el );
//	Add (cons) a list element el to the start of list l, modifying l.
//
void list_add( list l, LIST_ELEMENT el ) {
  assert( l != NULL );
  l->data = listdata_cons( el, l->data );
}


#define BIP_DEBUG


// list_addsorted( l, v );
//   Given a sorted list l (nil or not) and a value v,
//   add v to the list's data at the right place so that the list
//   REMAINS SORTED, modifying l.
//
void list_addsorted( list l, LIST_ELEMENT v ) {
  assert( l != NULL );
  listdata new = listdata_cons( v, NULL ); // need a new node with head v.
  if( l->data == NULL )
  {
    l->data = new;
    return;
  }

  assert( l->data != NULL );

  // must attach our new node at the right place:
  if( l->cmpf( l->data->head, v ) >= 0 ) { // if first list element >= v,
    new->tail = l->data;                   // add the new node at the front
    l->data = new;
    return;
  }

  assert( l->cmpf( l->data->head, v ) < 0 );

  // otherwise we find THE BEFORE INSERTION POINT bip:
  // - a node whose head h is <= v (bip->head <= v) and
  // - EITHER it is the very last node (bip->tail == NULL)
  //   or the following node's head (bip->tail->head) > v)
  listdata bip;

  // Task 2c: WRITE YOUR OWN CODE HERE

  for (bip=l->data; bip->tail != NULL && l->cmpf( bip->tail->head, v ) <= 0; bip = bip->tail) {}

  assert( bip != NULL );
  assert( l->cmpf( bip->head, v ) <= 0 );
  assert( bip->tail == NULL || l->cmpf( bip->tail->head, v ) > 0 );

  #ifdef BIP_DEBUG
  char buf[1024];
  l->spef( buf, bip->head );
  printf( "debug: addsorted(v=%p): bip is %p, head %s, tail %p\n",
    v, bip, buf, bip->tail );
  #endif

  // add the new node after the insertion point
  new->tail = bip->tail;
  bip->tail = new;
}



// LIST_ELEMENT found = list_find( l, value );
//	Find a given value in a given list; return either the
//	found value, or NULL.  Note: don't assume that the list
//	is ordered (ie. built by list_addsorted())
//
LIST_ELEMENT list_find( list l, LIST_ELEMENT value )
{
  assert( l != NULL );
  listdata ld;
  for( ld=l->data;
       ld!=NULL && l->cmpf( ld->head, value ) != 0;
       ld=ld->tail ) /*EMPTY*/;
  if( ld==NULL ) return NULL;
  return ld->head;
}


// int len = list_len( l );
//   find and return the length of the list l.
//
int list_len( list l ) {
  assert( l != NULL );
  int len = 0;
  for( listdata ld = l->data; ld != NULL; ld = ld->tail ) {
    len++;
  }
  return len;
}


// list extractor: deliberately inefficient, only intended for testing


// LIST_ELEMENT el = list_nth( n, l );
//   extract the nth element from the list.
//   abort if list doesn't HAVE n elements.
//
LIST_ELEMENT list_nth( int n, list l )
{
  assert( l != NULL );
  listdata p = l->data;
  for( ; n; n-- ) {
    assert( p != NULL );
    p = p->tail;
  }
  assert( p != NULL );
  return p->head;
}


// list_print( f, l );
//   print list l to file f as []-wrapped csv data
//   here we assume that no single element will occupy more than
//   1024 characters, hence the char buf[1024] variable
//
void list_print( FILE *f, list l ) {
  assert( l != NULL );
  listdata     ld   = l->data;
  list_sprintf spef = l->spef;
  char buf[1024];
  fputs( "[ ", f );
  while( ld != NULL ) {
    (*spef)( buf, ld->head );
    fprintf( f, "%s", buf );
    ld = ld->tail;
    if( ld != NULL ) fputs( ", ", f );
  }
  fputs( " ]", f );
}


// list_sprint( sbuf, l );
//   "string print" list l to char sbuf[] as []-wrapped csv data
//   here we simply assume that the buffer is big enough..
//   if we had a dynamic string, this could be used here..
//   we also assume that no single element will occupy more than
//   1024 characters, hence the char buf[1024] variable
//
void list_sprint( char *s, list l ) {
  assert( l != NULL );
  strcpy( s, "[ " );
  listdata ld = l->data;
  list_sprintf spef = l->spef;
  char buf[1024];
  while( ld != NULL ) {
    (*spef)( buf, ld->head );
    strcat( s, buf );
    ld = ld->tail;
    if( ld != NULL ) strcat( s, ", " );
  }
  strcat( s, " ]" );
}


// list_foreach( l, &cbfunc, &state );
//   list iterator: iterate over l, invoking the given cbfunc() once per
//   element, passing cbfunc() the list element, and the state pointer,
//   usually this is a pointer to some kind of struct, but it could be even
//   simpler, eg. a pointer to an int.
//   The cbfunc and the caller of foreach_list() must agree what the state
//   pointer really points to, and must conspire to implement this agreement.
//   The cbfunc() usually typecasts the state pointer to a ptr-to-what-it-
//   really-is, and then uses/updates various fields in the real state.
//
void list_foreach( list l, list_foreachf cb, void *state ) {
  assert( l != NULL );
  for( listdata ld = l->data; ld != NULL; ld = ld->tail ) {
    (*cb)( ld->head, state );
  }
}


// list_free( l );
//   free all elements on the list, and the list itself.
//   Use the list's free-element-function (fef) to free each
//   element, if fef is not NULL
//
void list_free( list l ) {
  assert( l != NULL );
  listdata ld = l->data;
  list_freef fef = l->fef;
  while( ld != NULL ) {
    if( fef != NULL ) {
      (*fef)( ld->head );
    }
    listdata tail = ld->tail;
    free( ld );
    ld = tail;
  }
  free( l );
}
