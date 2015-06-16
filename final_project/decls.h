/* global variables */

extern DECLLIST *predefines;

/* initialization functions */

extern DECLLIST *add_type_decl    (TYPE *t, char *s, DECLLIST *list);
extern DECLLIST *add_const_decl   (EXPR *e, char *s, DECLLIST *list);
extern DECLLIST *add_special_decl (SPECIALWHICH sk, char *s, DECLLIST *list);

extern void init_decls (void);

/* checking functions */

extern void resolve_decl (DECL *d);

extern int check_decl      (DECL *d);

extern int check_proc_decl (PROCFUNCDEF *p);
extern int check_arg_decl  (FORMAL *f);
extern void check_procfunc (DECL *d);

extern void check_decls    (DECLLIST *dl);

/* allocation functions */

extern OBJECT *get_decl_object (DECL *d);
extern void alloc_decl  (DECL *d);
extern void alloc_decls (DECLLIST *dl);
