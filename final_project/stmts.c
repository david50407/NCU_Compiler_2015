# include "project.h"

/**********************************************************************/
/*                         Checking functions                         */
/**********************************************************************/

int check_assign_stmt (STMT *s)
{
  int ok = 1;
  STMTASSIGN *a = s->s.asn;
  EXPR *lhs;
  EXPR *rhs;
  assert (a);
  lhs = a->asn_var;
  rhs = a->asn_expr;
  assert (lhs && rhs);
  if (lhs->tag == ExprBind_) {
    /* handle special case of function result assignment */
    DECL *d = check_bind_use (lhs->e.bnd);
    if (d == 0) {
      /* the check_bind_use call will already have output an error message */
      ok = 0;
    }
    else if (d->tag == DeclProcFunc_) {
      if ((lhs->expr_type = d->d.proc->proc_result_type) == 0) {
	/* note that the line above also sets the type, preventing an */
	/* inappropriate recheck of the lhs expression later          */
	errmsg (s->lineno, "Cannot assign to procedure name");
	ok = 0;
      }
      else if (d != current_procfunc) {
	/* it's the wrong proc/func */
	errmsg (s->lineno, "Cannot assign to function other than current one");
	ok = 0;
      }
      else
	lhs->expr_rhs = 0;
    }
  }

  /* here the lhs may or may not have been handled */
  if (ok && lhs->expr_type == 0)
    ok &= check_expr (lhs, 0);

  ok &= check_expr (rhs, 1);

  if (ok && !(ok = are_assignable_types (lhs->expr_type, rhs->expr_type)))
    errmsg (s->lineno, "Incompatible type in assignment");

  if (!ok)
    lhs->expr_type = the_err_type; /* suppress code generation */

  return ok;
}

int check_if_stmt (STMT *s)
{
  int ok = 1;
  STMTIF *i = s->s.ifx;
  EXPR *test;
  STMT *thenstmt;
  STMT *elsestmt;
  assert (i);
  test = i->if_expr;
  thenstmt = i->if_then;
  assert (test && thenstmt);

  ok = check_expr (test, 1);
  if (ok && !(ok = is_bool_type (test->expr_type)))
    errmsg (s->lineno, "IF test expression must be boolean");

  ok &= check_stmt (thenstmt);

  if ((elsestmt = i->if_else))
    ok &= check_stmt (elsestmt);

  if (!ok)
    test->expr_type = the_err_type; /* to suppress code generation */

  return ok;
}

int check_armlabels (EXPRLIST *list, TYPE *t)
{
  int ok = 1;

  if (list == 0)
    return 0;
  for (; list; list = list->rest) {
    EXPR *e = list->this;
    if (!check_const (e))
      ok = 0;
    else if (!are_compatible_types (e->expr_type, t)) {
      errmsg (e->lineno, "CASE label type incompatible with CASE expression");
      ok = 0;
    }
    else if (!const_is_in_type (e, t)) {
      errmsg (e->lineno, "CASE label value out of range");
      ok = 0;
    }
  }
  return ok;
}

int check_caselist (CASELIST *list, TYPE *t)
{
  int ok = 1;
  CASELIST *c;
  EXPRLIST *l;

  for (c = list; c; c = c->rest) {
    CASEARM *a = c->this;
    ok &= check_armlabels (a->arm_labels, t);
    ok &= check_stmt (a->arm_stmt);
  }

  if (ok) {
    /* check for duplicate values */
    for (c = list; c; c = c->rest)
      for (l = c->this->arm_labels; l; l = l->rest) {
	CASELIST *cc = c;
	EXPRLIST *ll = l;
	while (1) {
	  if ((ll = ll->rest) == 0) {
	    if ((cc = cc->rest))
	      ll = cc->this->arm_labels;
	    if (ll == 0)
	      break;
	  }
	  if (const_equal (l->this, ll->this)) {
	    errmsg (l->this->lineno, "Duplicate case label");
	    ok = 0;
	  }
	}
      }
  }
  return ok;
}

int check_case_stmt (STMT *s)
{
  int ok = 1;
  STMTCASE *c = s->s.cas;
  EXPR *e;
  assert (c);
  e = c->cas_expr;
  assert (e);

  ok = check_expr (e, 1);

  if (ok && !(ok = is_ordinal_type (e->expr_type)))
    errmsg (s->lineno, "CASE expression has inappropriate type");

  ok = ok && check_caselist (c->cas_arms, e->expr_type);

  if (!ok)
    e->expr_type = the_err_type; /* suppress code generation */

  return ok;
}

int check_while_stmt (STMT *s)
{
  int ok = 1;
  STMTWHILE *w = s->s.wh;
  EXPR *test;
  assert (w);
  test = w->wh_expr;
  assert (test);

  ok = check_expr (test, 1);

  if (ok && !(ok = is_bool_type (test->expr_type)))
    errmsg (s->lineno, "WHILE expression must be boolean");

  ok &= check_stmt (w->wh_stmt);

  if (!ok)
    test->expr_type = the_err_type; /* suppress code generation */

  return ok;
}

int check_repeat_stmt (STMT *s)
{
  int ok = 1;
  STMTREPEAT *r = s->s.rpt;
  EXPR *test;
  assert (r);
  test = r->rpt_expr;
  assert (test);

  ok = check_expr (test, 1);

  if (ok && !(ok = is_bool_type (test->expr_type)))
    errmsg (s->lineno, "REPEAT expression must be boolean");

  ok &= check_stmtlist (r->rpt_stmts);

  if (!ok)
    test->expr_type = the_err_type; /* suppress code generation */

  return ok;
}

int check_for_stmt (STMT *s)
{
  int ok = 1;
  STMTFOR *f = s->s.forx;
  BINDING *name;
  DECL *d;
  EXPR *init;
  EXPR *to;
  TYPE *t = 0;
  assert (f);
  name = f->for_name;
  init = f->for_init;
  to = f->for_to;
  assert (name && init && to);

  ok = ((d = check_bind_use (name)) != 0);

  if (ok && !(ok = (d->tag == DeclVar_)))
    errmsg (s->lineno, "FOR name must be a variable");

  if (ok && !(ok = (d->decl_symtab->symtab_plevel == proc_level)))
    errmsg (s->lineno, "FOR variable must be local to current procedure");

  if (ok && !(ok = is_ordinal_type (t = d->d.var->var_type)))
    errmsg (s->lineno, "FOR variable must be of an ordinal type");

  ok &= check_expr (init, 1);
  if (ok && !(ok = are_compatible_types (t, init->expr_type)))
    errmsg (s->lineno,
	    "FOR variable and init expression have incompatible types");

  ok &= check_expr (to, 1);
  if (ok && !(ok = are_compatible_types (t, to->expr_type)))
    errmsg (s->lineno,
	    "FOR variable and to expression have incompatible types");

  ok &= check_stmt (f->for_stmt);

  if (!ok)
    init->expr_type = the_err_type; /* suppress code generation */

  return ok;
}

int check_stmt (STMT *s)
{
  int ok = 1;

  if (s) {
    switch (s->tag) {
    case StmtAssign_:
      ok = check_assign_stmt (s);
      break;
    case StmtCompound_:
      ok = check_stmtlist (s->s.comp);
      break;
    case StmtIf_:
      ok = check_if_stmt (s);
      break;
    case StmtWhile_:
      ok = check_while_stmt (s);
      break;
    case StmtEmpty_:
      break;
    case StmtCase_:
      ok = check_case_stmt (s);
      break;
    case StmtRepeat_:
      ok = check_repeat_stmt (s);
      break;
    case StmtFor_:
      ok = check_for_stmt (s);
      break;
    case StmtCall_: {
      EXPR *e = s->s.call;
      assert (e);
      ok = 0;
      switch (e->tag) {
      case ExprBind_:
	ok = check_call (e, e->e.bnd, 0, 0);
	break;
      case ExprCall_:
	ok = check_call_expr (e, 0);
	break;
      default:
	errmsg (s->lineno, "Ill formed call statement");
	ok = 0;
	break;
      }
      break;
    }
    default:
      errmsg (s->lineno, "Unimplemented kind of statement");
      break;
    }
  }
  return ok;
}

int check_stmtlist (STMTLIST *stmts)
{
  int ok;
  for (ok = 1; stmts; stmts = stmts->rest)
    ok &= check_stmt (stmts->this);
  return ok;
}

/**********************************************************************/
/*               Allocation/folding functions                         */
/**********************************************************************/

void alloc_fold_stmt (STMT *s)
{
  if (s == 0) return;
  switch(s->tag) {
  case StmtEmpty_:
    break;
  case StmtAssign_: {
    STMTASSIGN *a = s->s.asn;
    EXPR *lhs, *rhs;
    assert (a);
    lhs = a->asn_var;
    rhs = a->asn_expr;
    assert (lhs && rhs);
    alloc_fold_assign (lhs, rhs);
    break;
  }
  case StmtCompound_:
    alloc_fold_stmts (s->s.comp);
    break;
  case StmtCall_:
    alloc_fold_expr (s->s.call);
    break;
  case StmtIf_: {
    STMTIF *sif = s->s.ifx;
    assert (sif);
    alloc_fold_expr (sif->if_expr);
    alloc_fold_stmt (sif->if_then);
    alloc_fold_stmt (sif->if_else);
    break;
  }
  case StmtCase_: {
    STMTCASE *sc = s->s.cas;
    CASELIST *cl;
    assert (sc);
    alloc_fold_expr (sc->cas_expr);
    for (cl = sc->cas_arms; cl; cl = cl->rest)
      if (cl->this) {
	alloc_fold_exprlist (cl->this->arm_labels);
	alloc_fold_stmt     (cl->this->arm_stmt);
      }
    break;
  }
  case StmtWhile_: {
    STMTWHILE *sw = s->s.wh;
    assert (sw);
    alloc_fold_expr (sw->wh_expr);
    alloc_fold_stmt (sw->wh_stmt);
    break;
  }
  case StmtRepeat_: {
    STMTREPEAT *sr = s->s.rpt;
    assert (sr);
    alloc_fold_expr  (sr->rpt_expr);
    alloc_fold_stmts (sr->rpt_stmts);
    break;
  }
  case StmtFor_: {
    STMTFOR *sf = s->s.forx;
    OBJECT *o = 0;
    assert (sf);
    alloc_fold_expr (sf->for_init);
    alloc_fold_expr (sf->for_to  );
    if (sf->for_to->expr_obj->tag == ObjMem_) {
      o = alloc_temp (sf->for_to->expr_type, 0);
      o->o.mem->mem_temp = 100;
      sf->for_limit_obj = o;
    }
    alloc_fold_stmt (sf->for_stmt);
    if (o) {
      o->o.mem->mem_temp = 1;
      free_temp (o);
    }
    break;
  }
  default:
    assert (0);
  }
}

void alloc_fold_stmts (STMTLIST *sl)
{
  for ( ; sl; sl = sl->rest) 
    alloc_fold_stmt (sl->this);
} 
