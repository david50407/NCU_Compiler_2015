#include <stdio.h>
char* cons_string(const char* s1, const char* s2, char* buffer) {
	char* ptr = &(buffer[0]);
	while(*s1 != '\0') {
		*ptr = *s1;
		++ptr;
		++s1;
	}
	while(*s2 != '\0') {
		*ptr = *s2;
		++ptr;
		++s2;
	}
	/* Remember to put \0 in the end of string. */
	*ptr = '\0';
	return buffer;
}
int main(int argc, char** argv) {
	/* Should prepare the space here, 
	 * or it will be released after exit `cons_string`. */
	char new_string[1024] = {'\0'};
	char* a = "Play";
	char* b = "Station";
	/* Initialization of pointer cannot give a value. */
	char* c;
	c = cons_string(a, b, new_string);
	printf("%s\n", c);
	/* return EXIT_SUCCESS */
	return 0;
}
