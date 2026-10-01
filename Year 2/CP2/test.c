#include <stdlib.h>

int main() {
	float *v;
	v = (float *) malloc(10*sizeof(float));
	if (v==NULL) { PANIC("malloc failed"); }

	free(v);

	return 0;
}
