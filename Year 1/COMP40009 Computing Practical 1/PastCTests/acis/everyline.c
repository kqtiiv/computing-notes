// everyline.[ch] module:
//	Provided I/O support functions, most notably one to read every line
//	of a file and invoke a callback with that line.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "everyline.h"

// int n = foreveryline( filename, eachlinef );
//	open the given filename, read every line from that a file,
//	remove the trailing newline (if present) and invoke the given
//	callback eachlinef with the filename, the line number and the line.
//	Return the number of lines read, -1 if the file can't be opened.
//
int foreveryline( char *filename, everylinecb eachlinef )
{
	// TASK 1: IMPLEMENT THIS
	FILE *in = fopen(filename, "r");
	if (in == NULL) {
		fprintf(stderr, "Unable to open file.\n");
		return -1;
	}

	line buffer;
	int line_number = 0;

	while (fgets(buffer, sizeof(buffer), in) != NULL) {
		if (buffer[strlen(buffer)-1] == '\n') {
			buffer[strlen(buffer)-1] = '\0';
		}
		eachlinef(filename, line_number, buffer);

		line_number++;
	} 

	fclose(in);
	return line_number;
}

