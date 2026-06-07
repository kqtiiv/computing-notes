#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include "io.h"
#include "assert.h"

extern int read_split_line( FILE * in, char * buf, int maxlen, char splitch, char ** data, int maxfields ){
  if (!read_line(in, buf, maxlen))
    return -1;
  char* line = strdup(buf);
  assert(line);
  const int num_fields = split_line(line, splitch, maxfields, data);
  free(line);
  return num_fields;
}

extern bool read_line( FILE * in, char * buf, int maxlen ){
  char* ret = fgets(buf, maxlen, in);
  if (ret == NULL)
    return false;
  size_t len = strlen(buf);
  if (buf[len - 1] == '\n'){
    buf[len - 1] = '\0';
  } else {
    fscanf(in, "%*[^\n]%*c");
  }
  return true;
}

extern int split_line( char * line, char splitch, int maxfields, char ** data ){
  int size;
  char splitstr[2] = {splitch};
  char* tok = strtok(line, splitstr);
  for (size = 0; tok && size < maxfields;){
    data[size++] = strdup(tok);
    assert(data[size - 1]);
    tok = strtok(NULL, splitstr);
  }
  return size;
}


extern void free_fields( char ** fields, int nel ){
  for (int i = 0; i < nel; ++i)
    free(fields[i]);
}
