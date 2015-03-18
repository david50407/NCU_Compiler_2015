#include <stdlib.h>

const void __swap(char* a, char* b) {
	char c = *a;
	*a = *b;
	*b = c;
}

/* Impl. Gnome sort. */
void mysort(void* base, size_t num, size_t size,
		int (*compar)(const void*, const void*)) {
	char *start;
	int i = 0, j;
	start = (char*)base;
	while (i < num - 1) {
		if (compar((void*)(start + i * size), (void*)(start + (i + 1) * size)) > 0) {
			for (j = 0; j < size; ++j)
				__swap(start + i * size + j, start + (i + 1) * size + j);
			--i;
			if (i == -1) i = 0;
		} else {
			++i;
		}
	}
}

int compare(const void * a, const void * b) {
	return ( *(int*)a - *(int*)b );
}

int main() {
	int values[] = { 2, 1, 3, 5, 4 };
	mysort(values, 5, sizeof(int), compare);
	int i;
	for (i = 0; i < 5; i++)
		printf ("%d ", values[i]);
	return 0;
}

