#include <stdio.h>
static void swap(int* a, int* b) {
	int tmp = *a;
	*a = *b;
	*b = tmp;
}
int main(int argc, char** argv) {
	int a, b;
	scanf("%d%d", &a, &b);
	swap(&a, &b);
	printf("%d %d\n", a, b);
	return 0;
}

