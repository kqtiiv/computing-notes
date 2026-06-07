// department.c: a simple department structure - implementation

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


// department d = make_department( name );
//	Create a new department with no members
//
department make_department( char *name ) {
  department p = malloc(sizeof(struct department));
  assert( p != NULL );

  COPYSTR( p->name, name, DEPTNAMELEN );
  p->members = make_plist();

  return p;
}


// add_to_department( d, p );
//	Add person p to department d
//
void add_to_department( department d, person p ) {
  list_addsorted( d->members, p );
}


// department_sprint( buf, d );
//	String-print department d into preallocated buffer buf,
//      assuming that the buffer is big enough
//
void department_sprint( char *buf, department d ) {
  sprintf( buf, "dept %s, members ", d->name );
  list_sprint( buf + strlen(buf), d->members );
}


// department_print( out, d );
//	Print department d to file out
//
void department_print( FILE *out, department d ) {
  char buf[500];
  department_sprint( buf, d );
  fprintf( out, "%s", buf );
}


// department_free( d );
//	Free the given department, members and all
//
void department_free( department d ) {
  list_free( d->members );
  free( d );
}


// int result = department_compare( a, b );
//	qsort() compatible comparison function, comparing
//	departments by name.
int department_compare( const void *a, const void *b ) {
  department ap  = (department)a;
  department bp  = (department)b;
  int        cmp = strcmp( ap->name, bp->name );
  return cmp;
}


typedef struct {
  char *	      deptname;
  department_foreachf origcb;
  void *              origstate;
} deptstate;


// wrapcb( disguisedperson, disguiseddeptstate );
//	Wrapper callback function, invoked by department_foreach_member()
//	below, taking a disguised person and a disguised deptstate pointer,
//	and invoking the department foreach callback function with it's
//	original state.  The problem is that a member of a department doesn't
//	know the name of the department it belongs to, but the department
//	foreach member callback needs to receive the department, the member,
//	and a piece of state; this trick of nesting one state inside another
//	allows us to pass the deptname, original callback and original state
//	around.
//
static void wrapcb( void *p, void *ds ) {
  deptstate *s = (deptstate *)ds;
  s->origcb( s->deptname, (person)p, s->origstate );
}


// department_foreach_member( d, &cb, state );
//	Invoke cb( deptname, member, state ) for each member of d
//
void department_foreach_member( department d, department_foreachf cb, void *state ) {
  deptstate ds = { .deptname = d->name, .origcb = cb, .origstate = state };
  list_foreach( d->members, &wrapcb, &ds );
}
