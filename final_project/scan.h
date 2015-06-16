extern int    yylineno;

extern IDENT *id_find_insert (char *name);
extern TEXT  *text_find_insert (char *buf);
extern void   text_iterate (void proc (TEXT *));
extern void   scaninit (void);
extern void   write_token (int token);
extern int    yylex (void);
