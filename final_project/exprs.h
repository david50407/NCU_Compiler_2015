/* global variables */

extern EXPR *the_false_expr;
extern EXPR *the_true_expr ;
extern EXPR *the_nil_expr  ;

/* examination functions */

extern int const_equal      (EXPR *e1, EXPR *e2);
extern int get_const_num    (EXPR *e);
extern int const_is_in_type (EXPR *e, TYPE *t);

extern VALUE *get_constant_value (EXPR *e);

extern int   get_element_offset (EXPR *a, EXPR *i);
extern TYPE *get_element_type   (EXPR *a);
extern int   get_field_offset (EXPR *r, EXPR *f);
extern TYPE *get_field_type   (EXPR *r, EXPR *f);

/* initialization functions */

extern void init_exprs (void);

/* expanding functions */

extern void bind_const  (EXPR *e);
extern int  check_const (EXPR *e);

/* checking functions */

extern int check_bin_expr  (EXPR *e, int rhs);
extern int check_un_expr   (EXPR *e, int rhs);
extern int check_bind_expr (EXPR *e, int rhs);

extern int check_int_expr (EXPR *e);
extern int check_txt_expr (EXPR *e);
extern int check_nil_expr (EXPR *e);
extern int check_id_expr  (EXPR *e);

extern int check_special_args (int line, EXPRLIST *args, int rhs,
			       int need_some);
extern int check_call_expr (EXPR *e, int need_result);
extern int check_call (EXPR *e, BINDING *b, EXPRLIST *actuals, int need_result);
extern int check_expr (EXPR *e, int rhs);

/* allocation/folding functions */

extern void alloc_fold_exprlist (EXPRLIST *el);
extern void alloc_fold_expr     (EXPR     *e );
extern void alloc_fold_assign   (EXPR *l, EXPR *r);

extern int is_non_addr_expr (EXPR *e);

extern OBJECT *fold_binop_int_compare (int op, int il, int ir);
extern OBJECT *fold_binop_int_int     (int op, int il, int ir);
extern OBJECT *fold_binop_bool_bool   (int op, int il, int ir);
extern OBJECT *fold_binop_ord_ord     (int op, int il, int ir);
extern OBJECT *fold_binop_asn_asn     (int op, int il, int ir);
extern OBJECT *fold_binop (int op, EXPR *l, EXPR *r);

extern OBJECT *fold_unop_bool (int op, int val);
extern OBJECT *fold_unop_int  (int op, int val);
extern OBJECT *fold_unop (int op, EXPR *expr);

extern void fold_expr  (EXPR *e);
extern void needs_expr (EXPR *e);
extern void alloc_temp_and_reg (EXPR *e);
extern void free_temp_and_reg  (EXPR *e);
extern void spill_expr (EXPR *e);
extern void alloc_expr (EXPR *e, int will_spill);
