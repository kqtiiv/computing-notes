// department.h: a simple department ADT - interface

#define DEPTNAMELEN  25
typedef char deptnamestr[DEPTNAMELEN];

// a department structure: a name + list(person)
struct department
{
  deptnamestr name;
  list        members;		// list(person)
};

// our "department" ADT: a pointer to a struct department
typedef struct department *department;


// department callback function: invoked for each member of a department
typedef void (*department_foreachf)( char *deptname, person member, void *state );


extern department make_department( char * name );
extern void add_to_department( department d, person p );
extern void department_sprint( char * buf, department d );
extern void department_print( FILE * out, department d );
extern void department_free( department d );
extern int department_compare( const void * a, const void * b );
extern void department_foreach_member( department d, department_foreachf cb, void * state );
