#include <stdio.h>
void swap_string(char** a_string, char** b_string) {
	char* temp;
	temp = *a_string;
	*a_string = *b_string;
	*b_string = temp;
}
int main(int argc, char** argv) {
	// my pointers
	int *a_ptr, *b_ptr;
	int a[] = { 1, 2, 3, 4, 6, 5 };
	a_ptr = a;
	b_ptr = a_ptr + 1;
	// show values of
	// the 1st element and the 2nd element
	printf("%d %d\n", *a_ptr, *b_ptr);
	char *msg1 = "World";
	char *msg2 = "Hello";
	swap_string(&msg1, &msg2);
	// show "Hello World!"
	printf("%s %s!\n", msg1, msg2);
	return 0;
}
