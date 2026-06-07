// readdata.c: read the given data file

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

//#include "testutils.h"

#include "io.h"
#include "list.h"
#include "person.h"
#include "plist.h"
#include "department.h"
#include "dlist.h"
#include "ageinfo.h"


#define MAXFIELDS 5
#define STRSIZE 50


int main( int argc, char **argv )
{
	char line[STRSIZE];
	char *fields[MAXFIELDS];

	list dl = make_dlist();

	int nfields;
	while( (nfields = read_split_line( stdin, line, STRSIZE,
				',', fields, MAXFIELDS ) ) != -1 )
	{
		if( nfields != MAXFIELDS )
		{
			fprintf( stderr,
				"Warning: ignored <%s>, only %d fields\n",
				line, nfields );
		} else
		{
			#if 0
			for( int i=0; i<nfields; i++ )
			{
				printf( "debug: fields[%d] = %s\n", i, fields[i] );
			}
			#endif
			person p = make_person( fields[1], fields[2], fields[3],
				atoi(fields[4]) );

			department found = find_or_add_department( dl, fields[0] );
			assert( found != NULL );
			printf( "debug: existing %s dept is ", fields[0] );
			department_print( stdout, found );
			putchar( '\n' );

			printf( "debug: adding person : " );
			person_print( stdout, p );
			printf( " to department %s\n", fields[0] );

			add_to_department( found, p );

			printf( "debug: after adding person to %s, dept is ",
				fields[0] );
			department_print( stdout, found );
			putchar( '\n' );
		}
		free_fields( fields, nfields );
	}

	ageinfo a;
	calc_ageinfo( dl, &a );
	int ndepts = a.ndepts;

	printf( "\nList of departments and their members:\n\n" );
	dlist_print( stdout, dl );
	putchar( '\n' );

	printf( "\nSummary: %d departments:\n\n", ndepts );
	printf( "Average ages (per dept)\n\n" );

	for( int i=0; i<ndepts; i++ )
	{
		printf( "dept %s: avg age %g\n", a.deptname[i], a.avgage[i] );
	}
	free_ageinfo( &a );

	list_free( dl );

	return 0;
}
