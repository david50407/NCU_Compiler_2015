/* statement checking functions */

extern int check_assign_stmt (STMT *s);
extern int check_if_stmt     (STMT *s);
extern int check_while_stmt  (STMT *s);
extern int check_armlabels   (EXPRLIST *list, TYPE *t);
extern int check_caselist    (CASELIST *list, TYPE *t);
extern int check_case_stmt   (STMT *s);
extern int check_repeat_stmt (STMT *s);
extern int check_for_stmt    (STMT *s);

extern int check_stmt     (STMT *s);
extern int check_stmtlist (STMTLIST *stmts);
/* statement allocation/folding functions */

extern void alloc_fold_stmt     (STMT *s);
extern void alloc_fold_stmts    (STMTLIST *sl);
