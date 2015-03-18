[Compiler] C 語言入門練習 / 01
==

1. **<基礎題>** Chorld wrote a program to practice C pointer, but his program has
some bugs. Please debug for him. The program should do things
right and has no compile warnings (with compile option `-Wall`).

 ```
#include <stdio.h>
void swap_string(char* a_string, char* b_string) {
 int* temp = a_string;
 a_string = b_string;
 b_string = temp;
}
int main(int argc, char** argv) {
 // my pointers
 int* a_ptr, b_ptr;
 int a[] = { 1, 2, 3, 4, 6, 5 };
 a_ptr = &a;
 b_ptr = ++a_ptr;
 // show values of
// the 1st element and the 2nd element
 printf("%d %d\n", *a_ptr, *b_ptr);
 char msg1[] = "World";
 char msg2[] = "Hello";
 swap_string(msg1, msg2);
 // show "Hello World!"
 printf("%s %s!\n");
 return 0;
}
 ```

2. **<基礎題>** Fill the swap procedure to finish this program:

 ```
#include <stdio.h>
static void swap(int* a, int* b) {
 // Write your code here!
}
int main(int argc, char** argv) {
 int a, b;
 scanf("%d%d", &a, &b);
 swap(&a, &b);
 printf("%d %d\n", a, b);
 return 0;
}
 ```

 Compile your program into assembly language (You can use either GCC
or Clang). For GCC, the command is `gcc -S myprogram.c`. And then
compile it again with optimize option: `gcc –O2 -S myprogram.c`.
After reading the two compiled results, could you tell me what did the
optimizer do?

3. **<基礎題>** Is there any problem in this program? If your answer is yes, please
fix it.

 ```
#include <stdio.h>
char* cons_string(const char* s1, const char* s2) {
 char new_string[1024] = {'\0'};
 char* ptr = &(new_string[0]);
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
 return new_string;
}
int main(int argc, char** argv) {
 char* a = "Play";
 char* b = "Station";
 char* c = cons_string(a, b);
 printf("%s\n", c);
}
 ```

4. **<進階題>** The quick-sort process (`qsort()` in `<stdlib.h>`) is defined as:

 ```
void qsort (void* base, size_t num, size_t size,
 int (*compar)(const void*,const void*));
 ```

 Please write a procedure called mysort using same interface as qsort.
You can use follow program to test if your procedure is correct:

 ```
int compare(const void * a, const void * b) {
 return ( *(int*)a - *(int*)b );
}
int main() {
 int values[] = { 2, 1, 3, 5, 4 };
 mysort (values, 6, sizeof(int), compare);
int i;
 for (i = 0; i < 6; i++)
 printf ("%d ",values[n]);
 return 0;
}
 ```

 Excepted output is: `1 2 3 4 5 `.
