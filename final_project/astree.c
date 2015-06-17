# include "project.h"
# include "parse.tab.h"

PROGRAM *the_program = 0;

/* constructor functions for "top-level" structures */

PROGRAM *make_program (DECLLIST *dl, BLOCK *b)
{
  PROGRAM *pgm = anew (PROGRAM);
  pgm->prog_decls = dl;
  pgm->prog_block = b;
  return (the_program = pgm);
}

BLOCK *make_block (DECLLIST *dl, STMTLIST *s)
{
  /* Malloc new BLOCK */
  BLOCK *blk = anew (BLOCK);
  blk->block_decls = dl;
  blk->block_stmts = s;
  return blk;
}

/**********************************************************************/
/*                       General DECL constructors                    */
/**********************************************************************/

DECL *make_decl (DECLKIND k, IDENT *id)
{
  DECL *d = anew (DECL);

  /* set tag, id, and lineno always */
  d->tag     = k;
  d->lineno  = yylineno;
  d->decl_id = id;

  /* clear out remaining fields */
  d->d.con       = 0;
  d->decl_outer  = 0;
  d->decl_symtab = 0;
  d->decl_number = 0;

  return d;
}

DECLLIST *make_decllist (DECL *d, DECLLIST *dl)
{
  if (d == 0)
    return dl;
  else {
    DECLLIST *new = (DECLLIST *) anew (DECLLIST);
    new->this = d;
    new->rest = dl;
    return new;
  }
}

/* DECL helper constructors */

VARDEF *make_vardef (TYPE *t)
{
  VARDEF *v = anew (VARDEF);
  v->var_type = t;
  v->var_addr = 0;
  return v;
}

FORMAL *make_formal (MODEWHICH m, TYPE *t)
{
  FORMAL *f = anew (FORMAL);
  f->frm_mode = m;
  f->frm_type = t;
  f->frm_addr = 0;
  return f;
}

PROCFUNCDEF *make_proc (DECLLIST *dl, TYPE *t, BLOCK *blk)
{
  PROCFUNCDEF *proc = anew(PROCFUNCDEF);
  proc->proc_formals = dl;
  proc->proc_result_type = t;
  proc->proc_block = blk;
  return proc;
}

/* specific DECL constructors */

DECL *make_var_decl (IDENT *id, TYPE *t)
{
  DECL *d = make_decl (DeclVar_, id);
  d->d.var = make_vardef (t);
  return d;
}

DECL *make_const_decl (IDENT *id, EXPR *c)
{
  DECL *d = make_decl (DeclConst_, id);
  d->d.con = c;
  return d;
}

DECL *make_type_decl (IDENT *id, TYPE *t)
{
  DECL *d = make_decl (DeclType_, id);
  d->d.typ = t ;
  return d;
}

DECL *make_formal_decl (IDENT *id, MODEWHICH m, TYPE *t)
{
  DECL *d = make_decl (DeclFormal_, id);
  d->d.frm = make_formal (m, t);
  return d;
}

DECL *make_program_decl (IDENT *id)
{
  return make_decl (DeclProgram_, id);
}

DECL *make_special_decl (IDENT *id, SPECIALWHICH skind)
{
  DECL *d = make_decl (DeclSpecial_, id);
  d->d.spc = skind;
  return d;
}

DECL *make_procfunc_decl (IDENT *id, DECLLIST *dl, IDENT *tid, BLOCK *blk)
{
  DECL *d = make_decl (DeclProcFunc_, id);
  d->d.proc = make_proc (dl, (tid == NULL ? 0 : make_id_type (tid)), blk);
  return d;
}

/**********************************************************************/
/*                     General TYPE constructors                      */
/**********************************************************************/

TYPE *make_type (TYPEKIND k)
{
  TYPE *t = anew (TYPE);

  t->tag    = k;
  t->size   = 0;
  t->lineno = yylineno;

  t->t.bind = 0;

  return t;
}

/* TYPE helper constructors */

TYPESUBR *make_typesubr (EXPR *lo, EXPR *hi)
{
  TYPESUBR *s = anew (TYPESUBR);
  s->rng_lo   = lo;
  s->rng_hi   = hi;
  s->rng_base = 0;
  return s;
}

TYPEARRAY *make_typearray (TYPE *idx, TYPE *elt)
{
  TYPEARRAY *ta = anew (TYPEARRAY);
  ta->arr_index   = idx;
  ta->arr_element = elt;
  return ta;
}

/* specific TYPE constructors */

TYPE *make_bind_type (BINDING *b)
{
  TYPE *t = make_type (TypeIdent_);
  t->t.bind = b;
  return t;
}

TYPE *make_enum_type (BINDLIST *bl)
{
  TYPE *t = make_type (TypeEnum_);
  t->t.enu = bl;
  t->size = 4;
  return t;
}

TYPE *make_range_type (EXPR *lo, EXPR *hi)
{
  TYPE *t = make_type (TypeSubrange_);
  t->t.rng = make_typesubr (lo, hi);
  t->size = 4;
  return t;
}

TYPE *make_id_type (IDENT *id)
{
  return make_bind_type (make_binding (id));
}

TYPE *make_array_type (TYPE *idx, TYPE *elt)
{
  TYPE *t = make_type (TypeArray_);
  t->t.arr = make_typearray (idx, elt);
  return t;
}

TYPE *make_fieldlist_type (FIELDLIST *fl)
{
  TYPE *t = make_type (TypeRecord_);
  t->t.rec = fl;
	return t;
}

/**********************************************************************/
/*                     General Field List constructors                */
/**********************************************************************/

FIELDLIST *make_fieldlist (IDENT *id, TYPE *t, FIELDLIST *fl)
{
  FIELDLIST *list = anew (FIELDLIST);
	FIELD *field = anew (FIELD);
	list->this = field;
	list->this->field_id = id;
	list->this->field_type = t;
  list->rest = fl;
  return list;
}

/**********************************************************************/
/*                     General STMT constructors                      */
/**********************************************************************/

STMT *make_stmt (STMTKIND k)
{
  STMT *s = anew (STMT);
  s->tag    = k;
  s->lineno = yylineno;
  s->s.asn  = 0;
  return s;
}

/* STMT helper constructors */

STMTASSIGN *make_stmtassign (EXPR *v, EXPR *e)
{
  STMTASSIGN *a = anew (STMTASSIGN);
  a->asn_var  = v;
  a->asn_expr = e;
  return a;
}

STMTCASE *make_stmtcase (EXPR *expr, CASELIST *list)
{
  STMTCASE *c = anew (STMTCASE);
  c->cas_expr = expr;
  c->cas_arms = list;
  return c;
}

STMTWHILE *make_stmtwhile (EXPR *test, STMT *body)
{
  STMTWHILE *w = anew (STMTWHILE);
  w->wh_expr = test;
  w->wh_stmt = body;
  return w;
}

STMTIF *make_stmtif (EXPR *test, STMT *body, STMT *other)
{
  STMTIF *i = anew (STMTIF);
  i->if_expr = test;
  i->if_then = body;
	i->if_else = other;
  return i;
}

STMTLIST *make_stmtlist (STMT *s, STMTLIST *sl)
{
	/* Statement might be empty (STMT pointer will be NULL)*/
	if (s == NULL) return sl;
  STMTLIST *l = anew (STMTLIST);
  l->this = s;
  l->rest = sl;
  return l;
}

STMTREPEAT *make_stmtrepeat (EXPR *test, STMTLIST *sl)
{
  STMTREPEAT *r = anew (STMTREPEAT);
  r->rpt_expr = test;
  r->rpt_stmts = sl;
  return r;
}

/* specific STMT constructors */

STMT *make_assign_stmt (EXPR *v, EXPR *e)
{
  STMT *s = make_stmt (StmtAssign_);
  s->s.asn = make_stmtassign (v, e);
  return s;
}

STMT *make_call_stmt (EXPR *e)
{
  STMT *s = make_stmt (StmtCall_);
  s->s.call = e;
  return s;
}

STMT *make_whilestmt (EXPR *test, STMT *body)
{
  STMT *s = make_stmt (StmtWhile_);
  s->s.wh = make_stmtwhile (test, body);
  return s;	
}

STMT *make_compound_stmt (STMTLIST *sl)
{
  STMT *s = make_stmt (StmtCompound_);
  s->s.comp = sl;
  return s;
}

STMT *make_ifstmt (EXPR *test, STMT *body, STMT *other)
{
  STMT *s = make_stmt (StmtIf_);
  s->s.ifx = make_stmtif (test, body, other);
  return s;
}

STMT *make_casestmt (EXPR *expr, CASELIST *cl)
{
  STMT *s = make_stmt (StmtCase_);
  s->s.cas = make_stmtcase (expr, cl);
  return s;
}

STMT *make_repeatstmt (EXPR *expr, STMTLIST *sl)
{
  STMT *s = make_stmt (StmtRepeat_);
  s->s.rpt = make_stmtrepeat (expr, sl);
  return s;
}

/**********************************************************************/
/*                     General CASE constructors                      */
/**********************************************************************/

CASELIST *make_caselist (CASEARM *ca, CASELIST *cl)
{
  CASELIST *cas = anew (CASELIST);
  cas->this = ca;
  cas->rest = cl;
  return cas;
}

CASEARM *make_casearm (EXPRLIST *el, STMT *stmt)
{
  CASEARM *ca = anew (CASEARM);
  ca->arm_labels = el;
  ca->arm_stmt = stmt;
  return ca;
}

/**********************************************************************/
/*                     General EXPR constructors                      */
/**********************************************************************/

EXPR *make_expr (EXPRKIND k)
{
  EXPR *e = anew (EXPR);
  e->tag    = k;
  e->lineno = yylineno;

  e->e.un      = 0;
  e->expr_type = 0;
  e->expr_obj  = 0;

  e->expr_rhs   = 1;
  e->expr_later = 0;
  e->reg_count  = 0;
  e->reg_name   = 0;
  e->reg_spill  = 0;

  return e;
}

EXPRLIST *make_exprlist (EXPR *e, EXPRLIST *el)
{
  EXPRLIST *new = anew (EXPRLIST);
  new->this = e;
  new->rest = el;
  return new;
}

/* EXPR helper constructors */

EXPRBINARY *make_exprbinary (int op, EXPR *left, EXPR *right)
{
  EXPRBINARY *new = anew (EXPRBINARY);
  new->bin_op    = op;
  new->bin_left  = left;
  new->bin_right = right;
  return new;
}

EXPRUNARY *make_exprunary (int op, EXPR *expr)
{
  EXPRUNARY *new = anew (EXPRUNARY);
  new->un_op   = op;
  new->un_expr = expr;
  return new;
}

EXPRCALL *make_exprcall (BINDING *b, EXPRLIST *args)
{
  EXPRCALL *c = anew (EXPRCALL);
  c->call_name    = b;
  c->call_actuals = args;
  return c;
}

/* specific EXPR constructors */

EXPR *make_bin_expr (int op, EXPR *left, EXPR *right)
{
  EXPR *new = make_expr (ExprBinary_);
  new->e.bin = make_exprbinary (op, left, right);
  return new;
}

EXPR *make_un_expr (int op, EXPR *expr)
{
  EXPR *new = make_expr (ExprUnary_);
  new->e.un = make_exprunary (op, expr);
  return new; 
}

EXPR *make_int_expr (int n)
{
  EXPR *new = make_expr (ExprInt_);
  new->e.num = n;
  new->expr_type = the_int_type;
  new->expr_obj  = make_int_object (n);
  return new;
}

EXPR *make_txt_expr (TEXT *txt)
{
  EXPR *new = make_expr (ExprText_);
  new->e.txt = txt;
  new->expr_type = the_txt_type;
  new->expr_obj  = make_txt_object (txt);
  return new;
}

EXPR *make_bind_expr (IDENT *id)
{
  EXPR *e = make_expr (ExprBind_);
  e->e.bnd = make_binding (id);
  return e;
}

EXPR *make_id_expr (IDENT *id)
{
  EXPR *e = make_expr (ExprIdent_);
  e->e.id = id;
  return e;
}

EXPR *make_call_expr (IDENT *id, EXPRLIST *args)
{
  EXPR *e = make_expr (ExprCall_);
  e->e.call = make_exprcall (make_binding (id), args);
  return e;
}

/* general BINDING routines */

BINDING *make_binding (IDENT *id)
{
  BINDING *b = anew (BINDING);
  b->bind_id   = id;
  b->bind_decl = 0;
  b->lineno = yylineno;
  return b;
}

BINDLIST *make_bindlist (BINDING *b, BINDLIST *bl)
{
  BINDLIST *new = anew (BINDLIST);
  new->this = b;
  new->rest = bl;
  return new;
}

/**********************************************************************/
/* Checking routines                                                  */
/**********************************************************************/

void check_program (PROGRAM *p)
{
  if (!p) return;
  scope_level = -1;
  proc_level  = -1;
  symtab_counter = -1;
  enter_scope (predefines, 1);  /* so that we always have an outer scope */
  check_decls (predefines);
  p->prog_syminfo = enter_scope (p->prog_decls, 1);
  check_decls (p->prog_decls);
  check_block (p->prog_block);
  exit_scope ();
  /* Note: do _not_ pop the last scope! (messes up the level variables) */
}

void check_block (BLOCK *b)
{
  b->block_syminfo = enter_scope (b->block_decls, 0);
  check_decls (b->block_decls);
  check_stmtlist (b->block_stmts);
  exit_scope ();
}

/**********************************************************************/
/* Allocation/folding routines                                        */
/**********************************************************************/

void alloc_fold_program (PROGRAM *p)
{
  if (!p ) return;
  scope_level    = 0;
  proc_level     = 0;
  symtab_counter = 0; /* reset symtab_counter value */

  re_enter_scope (p->prog_syminfo);
  alloc_decls (p->prog_decls);
  alloc_fold_block (p->prog_block);
  exit_scope ();
}

void alloc_fold_block (BLOCK *b)
{
  re_enter_scope (b->block_syminfo);
  alloc_decls (b->block_decls);
  alloc_fold_stmts (b->block_stmts);  
  exit_scope ();
}

/* --------------- traverse Abstract Syntax Tree ------------------------ */

static FILE *treefile;
static int nspc;

void tree_print (char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  vfprintf (treefile, fmt, args);
  va_end (args);
}

void print_indent (void)
{
  tree_print ("%-*c", nspc, '\n');
}

void print_start (char *s)
{
  tree_print ("(%s", s);
  nspc += 2;
}

void print_finish ()
{
  tree_print (")");
  nspc -= 2;
}

void print_id (IDENT *id)
{
  if (id)
    tree_print ("|%s|", id->id_chars);
}

void print_bind (BINDING *b)
{
  if (b) {
    DECL *d = b->bind_decl;
    print_id (b->bind_id);
    if (d) {
      SYMTAB *s = d->decl_symtab;
      tree_print (", number: %d", d->decl_number);
      if (s)
	tree_print (", scope: %d", s->symtab_number);
    }
  }
}

void print_op (int op)
{
  tree_print ("op(");
  switch(op) {
  case OR_ : tree_print ("OR" ); break;
  case AND_: tree_print ("AND"); break;
  case NOT_: tree_print ("NOT"); break;
  case LE_ : tree_print ("<=" ); break;
  case GE_ : tree_print (">=" ); break;
  case NE_ : tree_print ("<>" ); break;
  case DIV_: tree_print ("DIV"); break;
  case MOD_: tree_print ("MOD"); break;
  default  : tree_print ("%c", op); break;
  }
  tree_print(")");
}

void print_program (PROGRAM *p)
{
  if (p) {
    print_start ("program");
    if (p->prog_syminfo)
      tree_print (" [offset=%d]", p->prog_syminfo->symtab_offset);
    print_indent ();
    print_decllist (p->prog_decls);
    print_indent ();
    print_block (p->prog_block);
    print_finish ();
    tree_print ("\n");
  }
}

void print_block (BLOCK *b)
{
  if (b) {
    print_start ("block");
    if (b->block_syminfo)
      tree_print (" [offset=%d]", b->block_syminfo->symtab_offset);
    if (b->block_decls) {
      print_indent ();
      print_decllist (b->block_decls);
    }
    if (b->block_stmts) {
      print_indent ();
      print_stmtlist (b->block_stmts);
    }
    print_finish ();
  }
}

void print_decllist (DECLLIST *dl)
{
  if (dl) {
    print_start ("decllist");
    for (; dl; dl = dl->rest) {
      print_indent ();
      print_decl (dl->this);
    }
    print_finish (")");
  }
}

void print_decl (DECL *d)
{
  if (d) {
    print_start ("decl->");
    switch (d->tag) {

    case DeclConst_:
      tree_print ("const: ");
      print_id (d->decl_id);
      print_indent ();
      print_expr (d->d.con);
      break;

    case DeclType_:
      tree_print ("type: ");
      print_id (d->decl_id);
      print_indent ();
      print_type (d->d.typ);
      break;

    case DeclVar_:
      tree_print ("variable: ");
      print_id (d->decl_id);
      print_indent ();
      print_type (d->d.var->var_type);
      if (d->d.var->var_addr) {
	print_indent ();
	print_obj (d->d.var->var_addr);
      }
      break;

    case DeclProcFunc_:
      tree_print (d->d.proc->proc_result_type ? "function: " : "procedure: ");
      print_id (d->decl_id);
      if (d->d.proc->proc_syminfo)
        tree_print (" [offset=%d]", d->d.proc->proc_syminfo->symtab_offset);
      print_indent ();
      print_formallist (d->d.proc->proc_formals);
      if (d->d.proc->proc_result_type) {
	print_indent ();
	print_type (d->d.proc->proc_result_type);
      }
      if (d->d.proc->proc_result_addr) {
	print_indent ();
	print_obj (d->d.proc->proc_result_addr);
      }
      if (d->d.proc->proc_top_label) {
	print_indent ();
	tree_print ("top label: L%d", d->d.proc->proc_top_label);
      }
      print_indent ();
      print_block (d->d.proc->proc_block);
      break;

    case DeclFormal_:
      tree_print ("formal: ");
      print_id (d->decl_id);
      print_indent ();
      print_formal (d->d.frm);
      break;

    case DeclProgram_:
      tree_print ("program: ");
      print_id (d->decl_id);
      break;

    case DeclSpecial_:
      switch (d->d.spc) {
      case SpecialReadln_ : tree_print ("special: READLN" ); break;
      case SpecialWrite_  : tree_print ("special: WRITE"  ); break;
      case SpecialWriteln_: tree_print ("special: WRITELN"); break;
      case SpecialNew_    : tree_print ("special: NEW"    ); break;
      }
      break;

    case DeclErr_:
      tree_print ("err:");
      break;

    }
    if (d->decl_number) {
      print_indent ();
      tree_print ("number: %d", d->decl_number);
      if (d->decl_symtab) {
	tree_print (", scope: %d", d->decl_symtab->symtab_number);
      }
    }
    print_finish ();
  }
}

void print_stmtlist (STMTLIST *sl)
{
  if (sl) {
    print_start ("stmtlist");
    for (; sl; sl = sl->rest) {
      print_indent ();
      print_stmt (sl->this);
    }
    print_finish ();
  }
}

void print_stmt (STMT *s)
{
  if (s) {
    print_start ("stmt->");

    switch (s->tag) {
    case StmtIf_:
      tree_print ("ifstmt:");
      print_indent ();
      print_expr (s->s.ifx->if_expr);
      print_indent ();
      print_stmt (s->s.ifx->if_then);
      if (s->s.ifx->if_else) {
	print_indent ();
	print_start ("else");
	print_indent ();
	print_stmt (s->s.ifx->if_else);
	print_finish ();
      }
      break;

    case StmtCase_:
      tree_print ("casestmt:");
      print_indent ();
      print_expr (s->s.cas->cas_expr);
      print_indent ();
      print_caselist (s->s.cas->cas_arms);
      break;

    case StmtWhile_:
      tree_print ("whilestmt:");
      print_indent ();
      print_expr (s->s.wh->wh_expr);
      print_indent ();
      print_stmt (s->s.wh->wh_stmt);
      break;

    case StmtRepeat_:
      tree_print ("repeatstmt:");
      print_indent ();
      print_expr (s->s.rpt->rpt_expr);
      print_indent ();
      print_stmtlist (s->s.rpt->rpt_stmts);
      break;

    case StmtFor_:
      tree_print ("forstmt: ");
      print_bind (s->s.forx->for_name);
      print_indent ();
      print_expr (s->s.forx->for_init);
      print_indent ();
      print_expr (s->s.forx->for_to);
      if (s->s.forx->for_limit_obj) {
	print_indent ();
	print_obj (s->s.forx->for_limit_obj);
      }
      print_indent ();
      tree_print ("(upward:%d)", s->s.forx->for_upward);
      print_indent ();
      print_stmt (s->s.forx->for_stmt);
      break;

    case StmtAssign_:
      tree_print (s->s.asn->asn_expr ? "assignstmt:" : "callstmt:");
      print_indent ();
      print_expr (s->s.asn->asn_var);
      if (s->s.asn->asn_expr) {
	print_indent ();
	print_expr (s->s.asn->asn_expr);
      }
      break;

    case StmtCompound_:
      tree_print ("compoundstmt:");
      print_indent ();
      print_stmtlist (s->s.comp);
      break;

    case StmtCall_:
      tree_print ("callstmt:");
      print_indent ();
      print_expr (s->s.call);
      break;

    case StmtEmpty_:
      tree_print ("emptystmt:");
      break;
    }
    print_finish ();
  }
}

void print_expr (EXPR *e)
{
  if (e) {
    print_start ("expr->");
    print_obj (e->expr_obj);
    tree_print ("[%s", (e->expr_rhs ? "rhs" : "lhs"));
    if (e->expr_later)
      tree_print ("/later");
    if (e->reg_count)
      tree_print ("/regcnt=%d", e->reg_count);
    if (e->reg_name)
      tree_print ("/reg=%s", reg_pname[e->reg_name]);
    if (e->reg_spill)
      tree_print ("/spill=%d", e->reg_spill);
    tree_print ("]");
    tree_print (" ");
    switch (e->tag) {
    case ExprBinary_:
      tree_print ("binexp: ");
      print_op (e->e.bin->bin_op);
      print_indent();
      print_expr (e->e.bin->bin_left);
      print_indent ();
      print_expr (e->e.bin->bin_right);
      break;

    case ExprUnary_:
      tree_print ("unexp: ");
      print_op (e->e.un->un_op);
      print_indent ();
      print_expr (e->e.un->un_expr);
      break;

    case ExprInt_:
      tree_print ("int: %d", e->e.num);
      break;

    case ExprText_:
      tree_print ("string: \'%s\'", e->e.txt->text_chars);
      break;

    case ExprNil_:
      tree_print ("nil:");
      break;

    case ExprBind_:
      tree_print ("binding: ");
      print_bind (e->e.bnd);
      break;

    case ExprIdent_:
      tree_print ("ident: ");
      print_id (e->e.id);
      break;

    case ExprCall_:
      tree_print ("call: ");
      print_bind (e->e.call->call_name);
      print_indent ();
      print_exprlist (e->e.call->call_actuals);
      break;

    case ExprErr_:
      tree_print ("err:");
      break;
    }

    if (e->expr_type) {
      print_indent ();
      tree_print ("expr_type->");
      print_type (e->expr_type);
    }
    print_finish ();
  }
}

void print_type (TYPE *t)
{
  if (t) {
    print_start ("type->");
    tree_print ("size=%d, ", t->size);
    tree_print ("kind=");
    switch (t->tag) {
    case TypeEnum_:
      tree_print ("enum:");
      print_indent ();
      print_bindlist (t->t.enu);
      break;

    case TypeSubrange_:
      tree_print ("subrange:");
      print_indent ();
      print_expr (t->t.rng->rng_lo);
      print_indent ();
      print_expr (t->t.rng->rng_hi);
      break;

    case TypeIdent_:
      tree_print ("ident: ");
      print_bind (t->t.bind);
      break;

    case TypeArray_:
      tree_print ("array:");
      print_indent ();
      print_type (t->t.arr->arr_index);
      print_indent ();
      print_type (t->t.arr->arr_element);
      break;

    case TypeRecord_:
      tree_print ("record:");
      print_indent ();
      print_fieldlist (t->t.rec);
      break;

    case TypePointer_:
      tree_print ("pointer:");
      print_indent ();
      print_type (t->t.ptr);
      break;

    case TypeText_:
      tree_print ("STRING");
      break;

    case TypeInt_:
      tree_print ("INTEGER");
      break;

    case TypeBool_:
      tree_print ("BOOLEAN");
      break;

    case TypeNil_:
      tree_print ("NIL");
      break;

    case TypeErr_:
      tree_print ("ERROR");
      break;
    }
    print_finish ();
  }
}

void print_formallist (DECLLIST *dl)
{
  if (dl) {
    print_start ("formallist");
    for (; dl; dl = dl->rest) {
      print_indent();
      print_start ("formal ");
      print_id (dl->this->decl_id);
      print_indent ();
      print_formal (dl->this->d.frm);
      print_finish ();
    }
    print_finish ();
  }
}

void print_formal (FORMAL *f)
{
  if (f) {
    tree_print ("(varmode=%d)", f->frm_mode);
    print_indent ();
    print_type (f->frm_type);
    if (f->frm_addr) {
      print_indent ();
      print_obj (f->frm_addr);
    }
  }
}

void print_caselist (CASELIST *cl)
{
  if (cl) {
    print_start ("caselist");
    for (; cl; cl = cl->rest) {
      print_indent ();
      print_labellist (cl->this->arm_labels);
      print_indent ();
      print_stmt (cl->this->arm_stmt);
    }
    print_finish ();
  }
}

void print_labellist (EXPRLIST *el)
{
  if (el) {
    print_start ("labellist");
    for (; el; el = el->rest) {
      print_indent ();
      print_expr (el->this);
    }
    print_finish ();
  }
}

void print_bindlist (BINDLIST *bl)
{
  if (bl) {
    print_start ("bindlist");
    for (; bl; bl = bl->rest) {
      print_indent ();
      print_bind (bl->this);
    }
    print_finish ();
  }
}

void print_fieldlist (FIELDLIST *fl)
{
  if (fl) {
    print_start ("fieldlist");
    for (; fl; fl = fl->rest) {
      print_indent ();
      print_field (fl->this);
    }
    print_finish ();
  }
}

void print_exprlist (EXPRLIST *el)
{
  if (el) {
    print_start ("exprlist");
    for (; el; el = el->rest) {
      print_indent ();
      print_expr (el->this);
    }
    print_finish ();
  }
}

void print_offset (int offset)
{
  print_start ("field offset: "); 
  tree_print ("%d", offset);
  print_finish ();
}

void print_field (FIELD *f)
{
  if (f) {
    print_start ("field ");
    print_id (f->field_id);
    print_indent();
    print_offset (f->field_offset);
    print_indent ();
    print_type (f->field_type);
    print_finish ();
  }
}

void print_ast (PROGRAM *p, char *treename)
{
  if (!(treefile = fopen (treename, "w+")))
    fprintf (stderr, "Could not write ast on %s; continuing\n", treename);
  else {
    print_program (p);
    fclose (treefile);
  }
}

void print_value (VALUE *v)
/* A print routine for testing and debugging */
{
  tree_print ("[value=");
  switch (v->tag) {
  case ValueInt_:
    tree_print ("%d", v->v.intx);
    break;
  case ValueStr_:
    tree_print ("\'%s\'", v->v.text->text_chars);
    break;
  default:
    tree_print ("Request to print unknown kind of value\n");
    break;
  }
  tree_print ("]");
}

void print_obj (OBJECT *o)
{
  if (o) {
    switch (o->tag) {
    case ObjValue_:
      print_value (o->o.val);
      break;
    case ObjMem_: {
      MEMORY *m = o->o.mem;
      tree_print ("[lev=%d,off=%d,ind=%d,tmp=%d]",
		  m->mem_plevel, m->mem_offset, m->mem_indirect, m->mem_temp);
      break;
    }
    default:
      tree_print ("[error]");
      break;
    }
  }
}
