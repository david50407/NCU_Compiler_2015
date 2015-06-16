/* global variables */

extern SYMTAB *current_symtab;
extern DECL   *current_procfunc;

extern int scope_level;	
extern int proc_level;
extern int symtab_counter ;

/* general functions */

extern SYMTAB *enter_scope (DECLLIST *decls, int new_plevel);
extern void re_enter_scope (SYMTAB *s);
extern void exit_scope     (void);
extern void enter_decl     (DECL *d);
extern void enter_decls    (DECLLIST *dl);
extern void re_enter_decl  (DECL *d);
extern void re_enter_decls (DECLLIST *dl);
extern void remove_decl    (DECL *d);
extern void remove_decls   (DECLLIST *dl);

extern BOOLEAN check_id_decl (IDENT *id, int lineno);
extern DECL   *lookup_id     (IDENT *id);
extern DECL   *check_bind_use (BINDING *b);
extern int alloc_variable (int amount);
extern int alloc_formal   (int amount);
