// person.h: a simple person ADT - interface

#define NAMELEN  24
#define TITLELEN 8

// our "person" ADT is a pointer to a struct person.
typedef struct person *person;

// a person structure: stores a title, name and age
struct person
{
  char title[TITLELEN];
  char firstname[NAMELEN];
  char lastname[NAMELEN];
  int  age;
};


#define COPYSTR(dst,src,maxlen) { int len = strlen(src);\
	strncpy( dst, src, maxlen-1 );\
	if( len >= maxlen ) (dst)[maxlen-1] = '\0';\
	}

extern person make_person( char * title, char * first, char * last, int age );
extern void person_sprint( char * buf, person p );
extern void person_print( FILE * out, person p );
extern void person_free( person p );
extern void person_uppercase_names( person p );
extern int person_compare( const void * a, const void * b );
