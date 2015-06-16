/* offsets in the linkage area */
# define STATIC_CHAIN 8
# define DYNAMIC_CHAIN 4
# define RETURN_ADDR 0

/* other useful values */
# define MAX_S_IMMED (1 << 15) /* maximum   signed immediate */
# define MAX_U_IMMED (1 << 16) /* maximum unsigned immediate */

extern int current_plevel ;

extern void gen_program           (PROGRAM *p);
extern void gen_program_start     (PROGRAM *p);
extern void gen_program_block_end (PROGRAM *p);
extern void gen_program_end       (PROGRAM *p);

extern void gen_block_body  (BLOCK *b);
extern void gen_block_procs (BLOCK *b);
extern void gen_proc_start (PROCFUNCDEF *p);
extern void gen_proc_end   (PROCFUNCDEF *p);
extern void gen_procfunc   (PROCFUNCDEF *p);

extern void gen_stmtlist (STMTLIST *sl);

extern void gen_assign (EXPR *te, EXPR *se);
extern void do_branch  (EXPR *E, int ontrue, int label);

extern void gen_stmt_assign (STMTASSIGN *sa);
extern void gen_stmt_if (STMTIF *si);
extern void gen_stmt_case (STMTCASE *sc);
extern void gen_stmt_while (STMTWHILE *sw);
extern void gen_stmt_repeat (STMTREPEAT *sr);
extern void gen_stmt_for (STMTFOR *sf);
extern void gen_stmt (STMT *S);

extern void gen_load_expr (int reg, EXPR *e);
extern void gen_load      (int reg, OBJECT *o);
extern void gen_load_addr (int reg, OBJECT *o);
extern void gen_store     (int reg, int scratch, EXPR *e);

extern int get_expr_in_reg (EXPR *e, int scratch);
extern int get_addr_in_reg (EXPR *e, int scratch);

extern char *get_bin_opcode (int op);
extern char *is_immed       (int op, EXPR *e);
extern char *is_left_immed  (int op, EXPR *e);

extern void gen_expr_subscript (EXPR *e, EXPR *l, EXPR *r);
extern void gen_expr_field     (EXPR *e, EXPR *l, EXPR *r);
extern void gen_expr_binop (int op, EXPR *l, EXPR *r, int reg);

extern void gen_expr_deref (EXPR *e, EXPR *eu);
extern void gen_expr_unop (int op, EXPR *e, int reg);

extern void gen_expr_call (EXPR *e);

extern void gen_expr (EXPR *e);
