// dlist.c: convenience module to make creating linked lists of depts easier

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "person.h"
#include "list.h"
#include "plist.h"
#include "department.h"
#include "dlist.h"


// d_sprint( buf, v );
//	wrapper for department_sprint
//
static void d_sprint( char *buf, void *v ) {
  department_sprint( buf, (department)v );
}


// d_free( v );
//	wrapper for department_free
//
static void d_free( void *v ) {
  department_free( (department)v );
}


// list l = make_dlist();
//	Make an empty linked list of departments
//
list make_dlist( void )
{
  list dl = make_list( &department_compare, &d_sprint, &d_free );
  return dl;
}


// add_department_to_dlist( dl, d );
//	Add a department d (with or without members) to dlist dl.
//
void add_department_to_dlist( list dl, department d ) {
  list_addsorted( dl, d );
}


// department found = find_department( dl, deptname );
//	Find the department called "deptname" in dl, returning the
//	department; or NULL if no such department exists.
//
department find_department( list dl, char *deptname )
{
  department searchd = make_department( deptname );
  LIST_ELEMENT ld = list_find( dl, searchd );
  department_free( searchd );
  return (department)ld;
}


// department d = find_or_add_department( dl, deptname );
//	Find the department called "deptname" in dl,
//	adding an empty department to dl if no such
//	department exists.
//	Return the department.
//
department find_or_add_department( list dl, char *deptname )
{
  department searchd = make_department( deptname );
  LIST_ELEMENT ld = list_find( dl, searchd );
  department d = (department)ld;
  if( d == NULL )
  {
	add_department_to_dlist( dl, searchd );
	d = searchd;
  } else
  {
	department_free( searchd );
  }
  return d;
}


// foreach state

typedef struct {
  char *              deptname;
  department_foreachf origcb;
  void *              origstate;
} dliststate;


// person_in_dept_cb( person, state );
//	Wrapper callback function, invoked below once per member of a
//	department, that takes a disguised person and a piece of disguised
//	dliststate, and invokes the department foreach callback function
//	with it's original state.
//
static void person_in_dept_cb( void *dperson, void *dds ) {
  person p = (person) dperson;
  dliststate *s = (dliststate *)dds;
  #if 0
  printf( "debug: person_in_dept_cb: dept=%s, member=", s->deptname );
  person_print( stdout, p );
  putchar('\n');
  #endif
  s->origcb( s->deptname, p, s->origstate );
}


// dept_in_dlist_cb( department, state );
//	Wrapper callback function, invoked below once per department,
//	that takes a disguised department and a piece of disguised dliststate,
//	altering the dliststate to store the current department's name.
//	and iterates over that department's members with person_in_dept_cb()
//	as the callback, and the altered dliststate as the state
//
static void dept_in_dlist_cb( void *ddept, void *dds ) {
  department d = (department) ddept;
  dliststate *s = (dliststate *)dds;
  s->deptname = d->name;
  #if 0
  printf( "debug: dlist_fe_cbfunc: dept=%s, copy=%s, about to foreach(members)\n",
  	d->name, s->deptname );
  #endif
  list_foreach( d->members, &person_in_dept_cb, s );
}


// dlist_foreach_member( dl, &cb, state );
//	Invoke cb( deptname, member, state ) for each member of each dept,
//	using dept_in_dlist_cb as the callback and a new piece of dlist state
//	as the state
//
void dlist_foreach_member( list dl, department_foreachf cb, void *state ) {
  dliststate s = { .origcb = cb, .origstate = state };
  list_foreach( dl, &dept_in_dlist_cb, &s );
}

void dlist_cb(char *deptname, person member, void *state) {
  FILE *out = (FILE *)state;

  assert( deptname != NULL );
  assert( member != NULL );

  fprintf(out, "dept %s, ", deptname);
  fputs("member ", out);
  person_print(out, member);
  fputs("\n", out);
}

// dlist_print( out, dl );
//	Print dlist dl to file out
//
void dlist_print( FILE *out, list dl )
{
  // list_print( out, dl );
  dlist_foreach_member(dl, dlist_cb, out);
}
