# include "project.h"

/**********************************************************************/
/*                          Global variables                          */
/**********************************************************************/

DECLLIST *predefines = 0;

/**********************************************************************/
/*                Declaration initialization functions                */
/**********************************************************************/

DECLLIST *add_type_decl (TYPE *t, char *s, DECLLIST *list)
{
  assert (t);
  return make_decllist (make_type_decl (id_find_insert (s), t), list);
}

DECLLIST *add_const_decl (EXPR *e, char *s, DECLLIST *list)
{
  assert (e);
  return make_decllist (make_const_decl (id_find_insert (s), e), list);
}

DECLLIST *add_special_decl (SPECIALWHICH sk, char *s, DECLLIST *list)
{
  return make_decllist (make_special_decl (id_find_insert (s), sk), list);
}

void init_decls (void)
{
  /* predefined ids : Integer, Boolean
                      False, True,
                      Readln, Write, Writeln, New
   */
  DECLLIST *l = 0;

  assert (predefines == 0);

  l = add_type_decl (the_int_type , "integer", l);
  l = add_type_decl (the_bool_type, "boolean", l);

  l = add_const_decl (the_false_expr, "false", l);
  l = add_const_decl (the_true_expr , "true" , l);

  l = add_special_decl (SpecialNew_    , "new"    , l);
  l = add_special_decl (SpecialReadln_ , "readln" , l);
  l = add_special_decl (SpecialWrite_  , "write"  , l);
  l = add_special_decl (SpecialWriteln_, "writeln", l);

  predefines = l;
}

/**********************************************************************/
/*                   Declaration checking functions                   */
/**********************************************************************/

void resolve_decl (DECL *d)
{
  /* skip processing if the decl was not entered */
  if (d->decl_number == 0)
    return;

  switch(d->tag) {
  case DeclConst_: {
    /* a constant must be fully resolvable now */
    EXPR *e = d->d.con;

    assert (e);
    bind_const (e);
    break;
  }
  case DeclType_: {
    /* most types, but not pointer types, must be fully resolvable */
    assert (d->d.typ);
    resolve_type (&(d->d.typ));
    break;
  }
  case DeclVar_: {
    /* must resolve the type of the variable */
    assert (d->d.var && d->d.var->var_type);
    resolve_type (&(d->d.var->var_type));
    break;
  }
  case DeclProcFunc_: {
    /* must resolve the argument and result type */
    PROCFUNCDEF *p = d->d.proc;
    int ok = 1;
    DECLLIST *formals;
    assert (p);
    for (formals = p->proc_formals; formals; formals = formals->rest) {
      DECL *f = formals->this;
      assert (f->tag == DeclFormal_);
      resolve_type (&(f->d.frm->frm_type));
      ok &= !is_err_type (f->d.frm->frm_type);
    }
    if (p->proc_result_type) {
      resolve_type (&(p->proc_result_type));
      if (!is_ordinal_type (p->proc_result_type) && !is_ptr_type(p->proc_result_type)) {
	ok = 0;
	errmsg (d->lineno, "function result type must be scalar, subrange or pointer");
      }
    }
    if (!ok)
      p->proc_result_type = the_err_type;
    break;
  }
  case DeclFormal_:
    /* must resolve the argument's type */
    assert (d->d.frm && d->d.frm->frm_type);
    resolve_type (&(d->d.frm->frm_type));
    break;
  default:
    break ;
  }
}

int check_decl (DECL *d)
{
  switch (d->tag) {
  case DeclConst_   : return check_const     (d->d.con);
  case DeclType_    : return check_type      (&(d->d.typ));
  case DeclVar_     : return check_type      (&(d->d.var->var_type));
  case DeclProcFunc_: return check_proc_decl (d->d.proc);
  case DeclFormal_  : return check_arg_decl  (d->d.frm);
  case DeclErr_     : return 0;
  default           : return 1;
  }
}

int check_proc_decl (PROCFUNCDEF *p)
{
  int ok     = 1;
  DECLLIST *dl;
  for (dl = p->proc_formals; dl; dl = dl->rest) {
    DECL *d = dl->this;
    ok &= check_decl (d);
  }
  if (p->proc_result_type) {
    ok &= check_type (&(p->proc_result_type));
  }
  return ok;
}

int check_arg_decl (FORMAL *f)
{
  return check_type (&(f->frm_type));
}

void check_procfunc (DECL *d)
{
  DECL *old = current_procfunc;
  PROCFUNCDEF *p;
  assert (d && d->tag == DeclProcFunc_);
  p = d->d.proc;
  assert (p);
  current_procfunc = d;
  p->proc_syminfo = enter_scope (p->proc_formals, 1);
  check_decls (p->proc_formals);
  check_block (p->proc_block);
  exit_scope ();
  current_procfunc = old;
}

void check_decls (DECLLIST *dl)
{
  DECLLIST *p;

  /* first: expand enumeration types, resolve identifiers, etc. */
  for (p = dl; p; p = p->rest)
    resolve_decl (p->this);

  /* now check the resolved declarations */
  for (p = dl; p; p = p->rest)
    check_decl (p->this);

  /* now process any nested procedures */
  for (p = dl; p; p = p->rest) {
    DECL *d = p->this;
    if (d->tag == DeclProcFunc_)
      check_procfunc (d);
  }
}

/**********************************************************************/
/*                   Declaration allocation functions                 */
/**********************************************************************/

OBJECT *get_decl_object (DECL *d)
{
  OBJECT *o = the_err_object;
  if (d) {
    switch (d->tag) {
    case DeclConst_:
      assert (d->d.con);
      o = d->d.con->expr_obj;
      break;
    case DeclVar_:
      assert (d->d.var);
      o = d->d.var->var_addr;
      break;
    case DeclFormal_:
      assert (d->d.frm);
      o = d->d.frm->frm_addr;
      break;
    case DeclProcFunc_:
      assert (d->d.proc);
      o = d->d.proc->proc_result_addr;
      break;
    default:
      break;
    }
  }
  return (o ? o : the_err_object);
}

void alloc_decl (DECL *d)
{
  switch (d->tag) {
  case DeclVar_: {
    VARDEF *v = d->d.var;
    assert (v && v->var_type);
    if (v->var_type != the_err_type)
      v->var_addr =
	make_mem_object (v->var_type, 0, 0,
			 current_symtab->symtab_plevel,
			 alloc_variable (v->var_type->size));
    break;
  }
  case DeclProcFunc_: {
    DECL *old = current_procfunc;
    PROCFUNCDEF *p = d->d.proc;
    TYPE *t;
    assert (p);

    current_procfunc = d;
    re_enter_scope (p->proc_syminfo);

    alloc_decls (p->proc_formals);
    if ((t = p->proc_result_type) && t != the_err_type)
      p->proc_result_addr =
	make_mem_object (t, 0, 0, current_symtab->symtab_plevel,
			 alloc_formal (t->size));
    p->proc_formals_result_size = p->proc_syminfo->symtab_offset;
    p->proc_top_label = choose_label ();
    alloc_fold_block (p->proc_block);

    exit_scope ();
    current_procfunc = old;

    break;
  }
  case DeclFormal_: {
    FORMAL *f = d->d.frm;
    TYPE *t;
    assert (f);
    if ((t = f->frm_type) != the_err_type) {
      MODEWHICH mode = f->frm_mode;
      f->frm_addr =
	make_mem_object (t, 0, (mode == ModeVar_),
			 current_symtab->symtab_plevel,
			 alloc_formal (mode == ModeVar_ ? 4 : t->size));
    }
    break;
  }
  default:
    break;
  }
}

void alloc_decls (DECLLIST *dl)
{
  /* ensure that all variables are allocated before processing inner scopes */
  DECLLIST *dd;
  for (dd = dl; dd; dd = dd->rest) {
    DECL *d = dd->this;
    if (d && d->tag != DeclProcFunc_)
      alloc_decl (d);
  }
  for (dd = dl; dd; dd = dd->rest) {
    DECL *d = dd->this;
    if (d && d->tag == DeclProcFunc_)
      alloc_decl (d);
  }
}
