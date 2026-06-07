//
//   io.h: IO-related utility functions, source not provided, so function
//	   comments are provided here, annotating the prototypes
//


//
// char buf[MAXLEN];
// char *fields[MAXFIELDS];
// int nfields = read_split_line( in, buf, MAXLEN, splitch, fields, MAXFIELDS );
//	Read a single line from in (a FILE * open for input) into buf[MAXLEN],
//	reading no more than MAXLEN characters.  Remove a trailing '\n' from
//	the buffer (if there is one). If the input line is longer than maxlen,
//	the REMAINDER OF THE LINE IS SILENTLY DISCARDED.
//	Return -1 if no line can be read.
//
//	Then split the line into up to MAXFIELDS splitch-separated fields,
//	storing them in fields[0..MAXFIELDS-1] and returning the
//	number of fields found.  Note that the fields[MAXFIELDS] array must be
//	declared by the caller, but that the fields[i] strings are malloc()d
//	by this routine, and should be free()d later by
//		free_fields( fields, nfields ).
//
//	Return the number of nfields found (or -1 if no line can be read).
//
extern int read_split_line( FILE * in, char * buf, int maxlen, char splitch, char ** data, int maxfields );


//
// bool lineread = read_line( in, buf, maxlen );
//	read a single line from in (a FILE * open for input)
//	into a character buffer (char *), reading no more than
//	maxlen characters, removing a trailing '\n' from the
//	buffer (if there is one).
//
//	return false if EOF, else true if there's a line.
//
//	If the input line is longer than maxlen, the REMAINDER OF
//	THE LINE IS SILENTLY DISCARDED.
//
//	In caller-space, buf must be a writable character buffer of
//	size >= maxlen, i.e. typically either a char [n]
//	or a malloc(n * sizeof(char)) chunk,
//	where (in both cases) n >= maxlen
//
extern bool read_line( FILE * in, char * buf, int maxlen );


//
// char line[LINELEN];
// char *fields[MAXFIELDS];
// int nfields = split_line( line, splitch, MAXFIELDS, fields );
//	Split the line[] into up to MAXFIELDS splitch-separated fields,
//	modifying line[[ in place - replacing each occurrence of splitch
//	with a string terminator ('\0') - and storing the fields that we
//	find in fields[0..MAXFIELDS-1], strdup()ing each field.
//	Return the number of fields found.
//
//	Note that the fields[MAXFIELDS] array must be declared by the caller,
//	but that the fields[i] strings are malloc()d by this routine, and
//	should be free()d later by
//		free_fields( fields, nfields ).
//
extern int split_line( char * line, char splitch, int maxfields, char ** data );


// free_fields( fields, nfields );
//	Free fields[0..nfields-1]
//
extern void free_fields( char ** fields, int nel );
