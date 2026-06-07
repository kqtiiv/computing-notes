//
// list.h: singly linked list-of-generic(void *) pointers - interface
//

#define LIST_ELEMENT void *

// our ADT: a list - incomplete type
struct list;
typedef struct list *list;

// "compare two elements" function pointer type
typedef int (*list_cmpf)( const LIST_ELEMENT a, const LIST_ELEMENT b );

// "sprint an element" function pointer type
typedef void (*list_sprintf)( char *buf, LIST_ELEMENT el );

// "free an element" function pointer type
typedef void (*list_freef)( LIST_ELEMENT el );

// A foreach callback function, taking the head and a generic state pointer
typedef void (*list_foreachf)( LIST_ELEMENT head, void *state );

extern list make_list( list_cmpf cmpf, list_sprintf spef, list_freef fef );
extern void list_add( list l, LIST_ELEMENT el );
extern void list_addsorted( list l, LIST_ELEMENT v );
extern LIST_ELEMENT list_find( list l, LIST_ELEMENT value );
extern int list_len( list l );
extern LIST_ELEMENT list_nth( int n, list l );
extern void list_print( FILE * f, list l );
extern void list_sprint( char * s, list l );
extern void list_foreach( list l, list_foreachf cb, void * state );
extern void list_free( list l );
