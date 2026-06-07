// dlist: convenience module to make creating linked lists of depts easier

extern list make_dlist( void );
extern void add_department_to_dlist( list dl, department d );
extern department find_department( list dl, char * deptname );
extern department find_or_add_department( list dl, char * deptname );
extern void dlist_foreach_member( list dl, department_foreachf cb, void * state );
extern void dlist_print( FILE * out, list dl );
