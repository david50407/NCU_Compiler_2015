%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
struct I {
		int count;
		char name[10];
};
struct T {
    int size;
    struct I element[256];
} ans;
int compare(const void *A, const void *B) {
    return strcmp(((struct I *)A)->name, ((struct I *)B)->name);
}
void print(struct T* ptr) {
    int i;
    for (i = 0; i < ptr->size; ++i) {
        if (ptr->element[i].count == 0)
          continue;
        printf("%s %d\n", ptr->element[i].name, ptr->element[i].count);
    }
}
%}
%union {
struct T tmp;
char name[10];
int count;
}
%type  <tmp> element element_e nexpr expr stmt proc
%token <count> COUNT
%token <name> ITEM
%token PLUS
%token EQL
%token GROUP_START
%token GROUP_END
%%
proc   : stmt EQL stmt {
           int i, j;
			     memcpy(&$$, &$1, sizeof(struct T));
           for (i = 0; i < $3.size; ++i) {
               char f = 0;
               for (j = 0; j < $$.size; ++j) {
                   if (strcmp($$.element[j].name, $3.element[i].name) == 0) {
                       $$.element[j].count -= $3.element[i].count;
                       f = 1;
                       break;
                   }
               }
               if (f == 0) {
                   strcpy($$.element[$$.size].name, $3.element[i].name);
                   $$.element[$$.size].count = -$3.element[i].count;
                   ++$$.size;
               }
           }
           qsort($$.element, $$.size, sizeof(struct I), compare);
           print(&$$);
			 }
		   ;
stmt   : nexpr {
			     memcpy(&$$, &$1, sizeof(struct T));
			 }
			 | nexpr PLUS stmt {
			     memcpy(&$$, &$1, sizeof(struct T));
           int i, j;
           for (i = 0; i < $3.size; ++i) {
               char f = 0;
               for (j = 0; j < $$.size; ++j) {
                   if (strcmp($$.element[j].name, $3.element[i].name) == 0) {
                       $$.element[j].count += $3.element[i].count;
                       f = 1;
                       break;
                   }
               }
               if (f == 0) {
                   strcpy($$.element[$$.size].name, $3.element[i].name);
                   $$.element[$$.size].count = $3.element[i].count;
                   ++$$.size;
               }
           }
       }
		   ;
nexpr  : expr {
			     memcpy(&$$, &$1, sizeof(struct T));
			 }
       | COUNT expr {
			     memcpy(&$$, &$2, sizeof(struct T));
           int i;
			     for (i = 0; i < $$.size; ++i)
               $$.element[i].count *= $1;
			 }
expr   : element {
			     memcpy(&$$, &$1, sizeof(struct T));
			 }
       | element expr {
			     memcpy(&$$, &$1, sizeof(struct T));
           int i, j;
           for (i = 0; i < $2.size; ++i) {
               char f = 0;
               for (j = 0; j < $$.size; ++j) {
                   if (strcmp($$.element[j].name, $2.element[i].name) == 0) {
                       $$.element[j].count += $2.element[i].count;
                       f = 1;
                       break;
                   }
               }
               if (f == 0) {
                   strcpy($$.element[$$.size].name, $2.element[i].name);
                   $$.element[$$.size].count = $2.element[i].count;
                   ++$$.size;
               }
           }
       }
			 ;
element: element_e {
			     memcpy(&$$, &$1, sizeof(struct T));
			 }
			 | element_e COUNT {
			     memcpy(&$$, &$1, sizeof(struct T));
           int i;
           for (i = 0; i < $$.size; ++i)
               $$.element[i].count *= $2;
       }
			 ;
element_e : GROUP_START expr GROUP_END {
			        memcpy(&$$, &$2, sizeof(struct T));
					}
					| ITEM {
              $$.size = 1;
              strcpy($$.element[0].name, $1);
              $$.element[0].count = 1;
          }
%%
int yyerror(char *err) {
	fprintf(stdout, "Invalid format\n");
	exit(0);
	return 0;
}
int main() {
	yyparse();
	return 0;
}
