# include "project.h"
# include "parse.tab.h"

/**********************************************************************/
/*                Some global constant expressions                    */
/**********************************************************************/

EXPR *the_false_expr = 0;
EXPR *the_true_expr  = 0;
EXPR *the_nil_expr   = 0;

/**********************************************************************/
/*                   Functions related to constants                   */
/**********************************************************************/

int const_equal (EXPR *e1, EXPR *e2)
{
  VALUE *v1;
  VALUE *v2;
  return ((v1 = get_constant_value (e1)) &&
	  (v2 = get_constant_value (e2)) &&
	  value_equal (v1, v2));
}

/* get_const_num should be applied only to ordinal constants! */
int get_const_num (EXPR *e)
{
  VALUE *v = get_constant_value (e);
  assert (v && v->tag == ValueInt_);
  return v->v.intx;
}

int const_is_in_type (EXPR *e, TYPE *t)
{
  if (get_constant_value (e) && are_compatible_types (e->expr_type, t)) {
    switch (t->tag) {
    case TypeEnum_:
    case TypeBool_:
    case TypeInt_ :
    case TypeText_:
    case TypeNil_ :
      return 1;
    case TypeSubrange_: {
      int n = get_const_num (e);
      return ((subrange_lo_num (t) <= n) && (n <= subrange_hi_num (t)));
    }
    default:
      break;
    }
  }
  return 0;
}

VALUE *get_constant_value (EXPR *e)
{
  /* designed to allow testing as well as obtaining the value */
  OBJECT *o;
  return (e && (o = e->expr_obj) && (o->tag == ObjValue_)) ? o->o.val : 0;
}

int get_element_offset (EXPR *a, EXPR *i)
{
  /* calculate offset of element i in array a */
  /* a negative result indicates index out of range */
  TYPE *t, *ti, *te;
  TYPEARRAY *aa;
  int offset, index, lo, hi;
  assert (a);
  t = a->expr_type;
  assert (t && t->tag == TypeArray_);
  aa = t->t.arr;
  assert (aa);
  ti = aa->arr_index;
  assert (ti);
  te = aa->arr_element;
  assert (te);
  index = get_const_num (i);
  lo = index_lo_num (ti);
  hi = index_hi_num (ti);
  offset = (index - lo) * te->size;
  if (index > hi)
    offset = -1;
  if (offset < -MAX_FRAME_SIZE || offset > MAX_FRAME_SIZE)
    errmsg (a->lineno, "Constant index too large");
  return offset;
}

TYPE *get_element_type (EXPR *a)
{
  /* get element type of array a */
  TYPE *t;
  TYPEARRAY *aa;
  assert (a);
  t = a->expr_type;
  assert (t && t->tag == TypeArray_);
  aa = t->t.arr;
  assert (aa);
  return aa->arr_element;
}

int get_field_offset (EXPR *r, EXPR *f)
{
  /* calculate offset of field f in record r */

  TYPE *t;
  FIELDLIST *fl;
  FIELD *ff;
  IDENT *id;
  assert (r);
  t = r->expr_type;
  assert (t && t->tag == TypeRecord_);
  fl = t->t.rec;
  assert (fl && f && f->tag == ExprIdent_);
  id = f->e.id;
  assert (id);
  ff = find_field (fl, id);
  assert (ff);
  return ff->field_offset;
}

TYPE *get_field_type (EXPR *r, EXPR *f)
{
  /* get type of field f in record r */

  TYPE *t;
  FIELDLIST *fl;
  FIELD *ff;
  IDENT *id;
  assert (r);
  t = r->expr_type;
  assert (t && t->tag == TypeRecord_);
  fl = t->t.rec;
  assert (fl && f && f->tag == ExprIdent_);
  id = f->e.id;
  assert (id);
  ff = find_field (fl, id);
  assert (ff);
  return ff->field_type;
}

/**********************************************************************/
/*                     Expression initialization                      */
/**********************************************************************/

void init_exprs (void)
{
  assert (the_bool_type && the_false_object && the_true_object);
  the_false_expr = make_expr (ExprInt_);
  the_true_expr  = make_expr (ExprInt_);
  the_false_expr->expr_type = the_bool_type;
  the_true_expr ->expr_type = the_bool_type;
  the_false_expr->expr_obj = the_false_object;
  the_true_expr ->expr_obj = the_true_object ;

  assert (the_nil_type);
  the_nil_expr = make_expr (ExprNil_);
  the_nil_expr->expr_type = the_nil_type;
  the_nil_expr->expr_obj  = the_nil_object;
}

/**********************************************************************/
/*                   Expression resolving functions                   */
/**********************************************************************/

void bind_const (EXPR *e)
{
  OBJECT *result = the_err_object;
  switch (e->tag) {
  case ExprBinary_:
  case ExprNil_:
  case ExprIdent_:
  case ExprCall_:
    assert (0); /* these cases should not occur! */
    break;
  case ExprUnary_: {
    EXPRUNARY *eu = e->e.un;
    OBJECT *o;
    bind_const (eu->un_expr);
    o = eu->un_expr->expr_obj;
    if (o->obj_type != the_int_type)
      errmsg (e->lineno, "A signed constant must refer to an integer value");
    else {
      switch (eu->un_op) {
      case '-':
	result = make_int_object (-(o->o.val->v.intx));
	break;
      case '+':
	result = o;
	break;
      default:
	assert (0);
      }
    }
    break;
  }
  case ExprInt_:
  case ExprText_:
    result = e->expr_obj;
    assert (result);
    break;
  case ExprBind_: {
    DECL *d;
    OBJECT *o;
    if ((d = check_bind_use (e->e.bnd))) {
      if (d->tag != DeclConst_)
	errmsg (e->lineno, "Identifier must be bound to a constant");
      else if (!(o = d->d.con->expr_obj))
	errmsg (e->lineno, "Constant may not involve forward reference");
      else {
	result = o;
	e->expr_type = d->d.con->expr_type;
      }
    }
    break;
  }
  default:
    break;
  }
  if (e->expr_obj == 0)
    e->expr_obj = result;
  if (e->expr_type == 0 && e->expr_obj)
    e->expr_type = e->expr_obj->obj_type;
}

int check_const (EXPR *e)
{
  if (e == 0)
    return 0;
  if (e->expr_type == 0) {
    assert (e->expr_obj == 0);
    bind_const (e);
  }
  return !is_err_type (e->expr_type);
}

/**********************************************************************/
/*                   Expression checking functions                    */
/**********************************************************************/

# define insist_rhs(rhs, lineno) ((rhs) ? 1 : \
  (errmsg (lineno, "Context demands a writable variable"), 0))

int check_bin_expr (EXPR *e, int rhs)
{
  EXPRBINARY *eb = e->e.bin;
  e->expr_type = the_err_type;
  assert (eb);
  {
    EXPR *l = eb->bin_left;
    EXPR *r = eb->bin_right;
    int op = eb->bin_op;
    assert (l && r);
    if (check_expr (l, rhs) && check_expr (r, 1)) {
      /* note that right expressions need never be writable */
      TYPE *lt = l->expr_type;
      TYPE *rt = r->expr_type;
      switch (op) {
      case AND_:
      case OR_:
	if (is_bool_type (lt) && is_bool_type (rt)) {
	  e->expr_type = the_bool_type;
	  return insist_rhs (rhs, e->lineno);
	}
	break;
      case '+' :
      case '-' :
      case '*' :
      case DIV_:
      case MOD_:
	if (are_compatible_types (lt, rt) && is_int_based_type (lt)) {
	  e->expr_type = the_int_type;
	  return insist_rhs (rhs, e->lineno);
	}
	break;
      case '<':
      case '>':
      case LE_:
      case GE_:
	if (are_compatible_types (lt, rt) && is_ordinal_type (lt)) {
	  e->expr_type = the_bool_type;
	  return insist_rhs (rhs, e->lineno);
	}
	break;
      case '=':
      case NE_:
	if (are_compatible_types (lt, rt) &&
	    (is_ordinal_type (lt) || is_ptr_type (lt))) {
	  e->expr_type = the_bool_type;
	  return insist_rhs (rhs, e->lineno);
	}
	break;
      case '.':
	if (lt->tag == TypeRecord_) {
	  FIELD *f;
	  IDENT *id = r->e.id;
	  assert (r->tag == ExprIdent_ && id);
	  if ((f = find_field (lt->t.rec, id))) {
	    e->expr_type = f->field_type;
	    return 1;
	  }
	  errmsg (e->lineno, "Bad field name %s in selection", id->id_chars);
	  return 0;
	}
	errmsg (e->lineno, "Attempt to select from non-record");
	return 0;
      case '[':
	if (lt->tag == TypeArray_) {
	  TYPEARRAY *ta = lt->t.arr;
	  assert (ta);
	  if (are_compatible_types (ta->arr_index, rt)) {
	    e->expr_type = ta->arr_element;
	    return 1;
	  }
	  errmsg (e->lineno, "Index type incompatible with array index type");
	  return 0;
	}
	errmsg (e->lineno, "Attempt to index non-array");
	return 0;
      default:
	break;
      }
      errmsg (e->lineno, "Illegal types of operands in binary expression");
    }
  }
  return 0;
}

int check_un_expr (EXPR *e, int rhs)
{
  EXPRUNARY *eu = e->e.un;
  e->expr_type = the_err_type;
  assert (eu);
  {
    EXPR *ex = eu->un_expr;
    if (check_expr (ex, 1)) {
      TYPE *et = ex->expr_type;
      switch (eu->un_op) {
      case NOT_:
	if (is_bool_type (et)) {
	  e->expr_type = the_bool_type;
	  return insist_rhs (rhs, e->lineno);
	}
	break;
      case '+':
      case '-':
	if (is_int_based_type (et)) {
	  e->expr_type = the_int_type;
	  return insist_rhs (rhs, e->lineno);
	}
	break;
      case '^':
	if (is_ptr_type (et)) {
	  TYPE *pt = et->t.ptr;
	  BINDING *b;
	  DECL *d;
	  assert (pt && pt->tag == TypeIdent_);
	  b = pt->t.bind;
	  assert (b);
	  d = b->bind_decl;
	  assert (d && d->tag == DeclType_);
	  e->expr_type = d->d.typ;
	  return 1;
	}
	errmsg (e->lineno, "Attempt to dereference non-pointer");
	return 0;
      default:
	break;
      }
      errmsg (e->lineno, "Incompatible operation in unary expression");
    }
  }
  return 0;
}

int check_bind_expr (EXPR *e, int rhs)
{
  BINDING *b = e->e.bnd;
  e->expr_type = the_err_type;
  assert (b);
  {
    DECL *d = check_bind_use (b);
    if (d) {
      switch (d->tag) {
      case DeclConst_:
	e->expr_type = d->d.con->expr_type;
	return insist_rhs (rhs, e->lineno);
      case DeclVar_:
	e->expr_type = d->d.var->var_type;
	return 1;
      case DeclFormal_:
	e->expr_type = d->d.frm->frm_type;
	return 1;
      case DeclProcFunc_:
	insist_rhs (rhs, e->lineno);
	/* the special case of assigning to function names to return function
	   results _must_ be handled elsewhere (e.g., with the assignment
	   statement) */
	return check_call (e, b, 0, 1);
      case DeclSpecial_:
	errmsg (e->lineno, "Inappropriate call of special function or procedure");
	return 0;
      case DeclProgram_:
	errmsg (e->lineno, "Meaningless to reference program identifier");
	return 0;
      case DeclType_:
	errmsg (e->lineno, "Attempt to use a type as an expression");
	return 0;
      default:
        errmsg (e->lineno, "Name bound to not yet implemented kind of thing");
	return 0;
      }
    }
  }
  return 0;
}

int check_int_expr (EXPR *e)
{
  e->expr_type = the_int_type;
  return 1;
}

int check_txt_expr (EXPR *e)
{
  e->expr_type = the_txt_type;
  return 1;
}

int check_nil_expr (EXPR *e)
{
  e->expr_type = the_nil_type;
  return 1;
}

int check_id_expr (EXPR *e)
{
  /* uses will be checked contextually (record selection); has no type */
  return 1;
}

int check_arguments (DECLLIST *formals, EXPRLIST *actuals, int lineno)
{
  int ok = 1;
  while (formals && actuals) {
    DECL *formal = formals->this;
    EXPR *actual = actuals->this;
    FORMAL *f = formal->d.frm;
    assert (formal->tag == DeclFormal_);

    switch (f->frm_mode) {
    case ModeValue_:
      if (!check_expr (actual, 1))
	ok = 0;
      else if (!are_assignable_types (f->frm_type, actual->expr_type)) {
	errmsg (lineno, "Value argument is not compatible with formal");
	ok = 0;
      }
      break;
    case ModeVar_:
      if (!check_expr (actual, 0))
	ok = 0;
      else if (f->frm_type != actual->expr_type) {
	errmsg (lineno, "Var argument and formal have different types");
	ok = 0;
      }
      break;
    default:
      assert (0);
    }

    formals = formals->rest;
    actuals = actuals->rest;
  }
  if (formals) {
    errmsg (lineno, "More formals than actuals in call");
    ok = 0;
  }
  else if (actuals) {
    errmsg (lineno, "More actuals than formals in call");
    ok = 0;
  }
  return ok;
}

int check_special_args (int line, EXPRLIST *args, int rhs, int need_some)
{
  int ok = 0;
  if (need_some && args == 0)
    errmsg (line, "Special procedure requires arguments");
  else
    for (ok = 1; args; args = args->rest) {
      assert (args->this);
      if (!check_expr (args->this, rhs))
	ok = 0;
      else if ((!is_ordinal_type (args->this->expr_type)) &&
	       (!is_txt_type     (args->this->expr_type))) {
	errmsg (line, "Can read/write only ordinal or text string values");
	ok = 0;
      }
    }
  return ok;
}

int check_call_expr (EXPR *e, int need_result)
{
  EXPRCALL *c = e->e.call;
  assert (c && c->call_name);
  return check_call (e, c->call_name, c->call_actuals, need_result);
}

int check_call (EXPR *e, BINDING *b, EXPRLIST *actuals, int need_result)
{
  int ok = 0;
  DECL *d;
  e->expr_type = the_err_type;
  if ((d = check_bind_use (b))) {
    switch (d->tag) {
    case DeclProcFunc_: {
      PROCFUNCDEF *p = d->d.proc;
      assert (p);
      if (need_result && p->proc_result_type == 0)
	errmsg (e->lineno, "Calling a procedure where a function is needed");
      else if (p->proc_result_type && need_result == 0)
	errmsg (e->lineno, "Calling a function where a procedure is needed");
      else if (check_arguments (p->proc_formals, actuals, e->lineno)) {
	e->expr_type = p->proc_result_type;
	ok = 1;
      }
      break;
    }
    case DeclSpecial_:
      if (need_result)
	errmsg (e->lineno, "Cannot call special procedure as function");
      else {
	switch (d->d.spc) {
	case SpecialReadln_:
	  ok = check_special_args (e->lineno, actuals, 0, 0); break;
	case SpecialWrite_:
	  ok = check_special_args (e->lineno, actuals, 1, 1); break;
	case SpecialWriteln_:
	  ok = check_special_args (e->lineno, actuals, 1, 0); break;
	case SpecialNew_:
	  if (actuals == 0)
	    errmsg (e->lineno, "NEW must have an argument");
	  else {
	    EXPR *a = actuals->this;
	    ok = check_expr (a, 0);
	    if (ok && !(ok = (a->expr_type->tag == TypePointer_)))
	      errmsg (e->lineno, "Argument of NEW must be a pointer");
	    if (actuals->rest) {
	      errmsg (e->lineno, "NEW takes a single argument");
	      ok = 0;
	    }
	  }
	}
	if (ok)
	  e->expr_type = 0;
      }
      break;
    default:
      errmsg (e->lineno, "Name in call not a procedure or function");
    }
  }
  if (ok && actuals == 0) {
    /* turn ok bindings into calls with no arguments */
    assert (e->tag == ExprBind_);
    e->tag = ExprCall_;
    e->e.call = make_exprcall (b, 0);
  }
  return ok;
}

int check_expr (EXPR *e, int rhs)
{
  assert (e);
  e->expr_rhs = rhs;
  switch (e->tag) {
  case ExprBinary_: return check_bin_expr  (e, rhs);
  case ExprUnary_ : return check_un_expr   (e, rhs);
  case ExprBind_  : return check_bind_expr (e, rhs);
  case ExprIdent_ : return check_id_expr (e);
  case ExprInt_ : return insist_rhs (rhs, e->lineno) && check_int_expr  (e);
  case ExprText_: return insist_rhs (rhs, e->lineno) && check_txt_expr  (e);
  case ExprNil_ : return insist_rhs (rhs, e->lineno) && check_nil_expr  (e);
  case ExprCall_: return insist_rhs (rhs, e->lineno) && check_call_expr (e, 1);
  default       : errmsg (e->lineno, "Malformed expression"); return 0;
  }
}

/**********************************************************************/
/*        Expression allocation/checking functions                    */
/**********************************************************************/

void alloc_fold_exprlist (EXPRLIST *el)
{
  for ( ; el; el = el->rest)
    alloc_fold_expr (el->this);
}
        
void alloc_fold_expr (EXPR *e)
{
  /* save means that the result should be saved to its memory home */

  fold_expr  (e);    /* perform constant folding */
  needs_expr (e);    /* analyze temporary/register needs of expression */
  alloc_expr (e, 0); /* allocate temporaries/registers in expression */
  free_temp_and_reg (e); /* free them, since result consumed outside */
}

void alloc_fold_assign (EXPR *l, EXPR *r)
{
  int lfirst, spill;

  fold_expr (l);
  fold_expr (r);
  needs_expr (l);
  needs_expr (r);
  lfirst = (l->reg_count >= r->reg_count);
  spill = (lfirst ? r : l)->reg_count >= ALLOC_REGS;
  alloc_expr ((lfirst ? l : r), spill);
  if (regs_available == 0 || spill)
    spill_expr (lfirst ? l : r);
  alloc_expr ((lfirst ? r : l), 0);
  (lfirst ? r : l)->expr_later = 1;
  free_temp_and_reg (lfirst ? r : l);
  free_temp_and_reg (lfirst ? l : r);
}

int is_non_addr_expr (EXPR *e)
{
  /* indicates whether the value of the expression is not an address */
  /* this is distinct from lvalue vs. rvalue; it is used to distinguish */
  /* register contents that are/should be addresses from those that are */
  /* values (e.g., integers); lvalues are always addresses, but so are */
  /* some rvalues ... */
  TYPE *t;
  if (e->expr_rhs == 0)
    return 0;
  assert (e);
  t = e->expr_type;
  assert (t);
  switch (t->tag) {
  case TypeArray_:
  case TypeRecord_:
  case TypeText_:
    return 0;
  default:
    return 1;
  }
}

OBJECT *fold_binop_int_compare (int op, int il, int ir)
{
  int val;
  switch (op) {
  case '=': val = (il == ir); break;
  case NE_: val = (il != ir); break;
  case '>': val = (il >  ir); break;
  case '<': val = (il <  ir); break;
  case GE_: val = (il >= ir); break;
  case LE_: val = (il <= ir); break;
  default:  return 0;
  }
  return (val ? the_true_object : the_false_object); 
}

OBJECT *fold_binop_int_int (int op, int il, int ir)
{
  switch (op) {
  case '+' : return make_int_object (il + ir);
  case '-' : return make_int_object (il - ir);
  case '*' : return make_int_object (il * ir);
  case DIV_: return (ir == 0) ? 0 : make_int_object (il / ir);
  case MOD_: return (ir == 0) ? 0 : make_int_object (il % ir);
  case '=' :
  case NE_ :
  case '>' :
  case '<' :
  case GE_ :
  case LE_ : return fold_binop_int_compare (op, il, ir);
  default  : return 0;
 }
}
	
OBJECT *fold_binop_bool_bool (int op, int il, int ir)
{
  int val ;

  switch (op) {
  case OR_ : val = (il | ir); break;
  case AND_: val = (il & ir); break;
  case '=' :
  case NE_ :
  case '>' :
  case '<' :
  case GE_ :
  case LE_ : return fold_binop_int_compare (op, il, ir);
  default  : return 0;
  }
  return (val ? the_true_object : the_false_object);
}
           
OBJECT *fold_binop_ord_ord (int op, int il, int ir)
{
  return fold_binop_int_compare (op, il, ir);
}

OBJECT *fold_binop_asn_asn (int op, int il, int ir)
{
  /* nil compared with nil (useless, but possible) */
  return fold_binop_int_compare (op, il, ir);
}     

OBJECT *fold_binop (int op, EXPR *l, EXPR *r)
{
  OBJECT *ol, *or;
  VALUE  *vl, *vr;
  fold_expr (l);
  fold_expr (r);
  ol = l->expr_obj;
  or = r->expr_obj;
  vl = get_constant_value (l);
  vr = get_constant_value (r);
  if (vl && vr) {
    TYPE *tl = ol->obj_type;
    TYPE *tr = or->obj_type;
    int il, ir;
    assert (vl && vr && tl && tr);
    if (vl->tag != ValueInt_ ||	vr->tag != ValueInt_)
      return the_err_object ;
    il = vl->v.intx;
    ir = vr->v.intx;
    if (is_int_based_type (tl) && is_int_based_type (tr)) 
      return fold_binop_int_int (op, il, ir);
    if (is_bool_type (tl) && is_bool_type (tr))
      return fold_binop_bool_bool (op, il, ir);
    if (is_ordinal_type (tl) &&	is_ordinal_type (tr)) /* for enum type */
      return fold_binop_ord_ord (op, il, ir);        
    if (are_assignable_types (tl, tr))
      return fold_binop_asn_asn (op, il, ir);
  }
  else if (ol) {
    /* fold address arithmetic where possible */
    switch (op) {
    case '[': {
      MEMORY *m = ol->o.mem;
      assert (m);
      if (m->mem_indirect == 0 && vr) {
	int offset = get_element_offset (l, r);
	if (offset >= 0)
	  return make_mem_object (get_element_type (l), 0, 0,
				  m->mem_plevel, m->mem_offset + offset);
	warning (r->lineno, "Warning: constant index out of range");
      }
      break;
    }
    case '.': {
      MEMORY *m = ol->o.mem;
      assert (m);
      if (m->mem_indirect == 0) {
	return make_mem_object (get_field_type (l, r), 0, 0,
				m->mem_plevel,
				m->mem_offset + get_field_offset (l, r));
      }
      break;
    }
    default:
      break;
    }
  }
  return 0;
}

OBJECT *fold_unop_bool (int op, int val)
{
  if (op == NOT_)
    return (val == 0 ? the_true_object : the_false_object);
  return 0;
}

OBJECT *fold_unop_int (int op, int val)
{
  switch (op) {
  case '+': return make_int_object ( val);
  case '-': return make_int_object (-val);
  default : break;
  }
  return 0;
}

OBJECT *fold_unop (int op, EXPR *expr)
{
  OBJECT *o;
  fold_expr (expr);
  if ((o = expr->expr_obj) && o->tag == ObjValue_) {
    TYPE *t = o->obj_type;
    VALUE *v = o->o.val;
    int val;
    assert (v);
    val = v->v.intx;
    if (is_int_type (t))
      return fold_unop_int (op, val);
    if (is_bool_type (t))
      return fold_unop_bool (op, val);
  }
  return 0;
}

void fold_expr (EXPR *e)
{
  if (e == 0 || e->expr_type == the_err_type) {
    e->expr_obj = the_err_object; 
    return;
  }
  else if (e->expr_obj)
    return;

  switch (e->tag) {
  case ExprBinary_: {
    EXPRBINARY *b = e->e.bin;
    assert (b);
    e->expr_obj = fold_binop (b->bin_op, b->bin_left, b->bin_right);
    break;
  }
  case ExprUnary_: {
    EXPRUNARY *u = e->e.un;
    assert (u);
    e->expr_obj = fold_unop (u->un_op, u->un_expr);
    break;
  }
    break;
  case ExprBind_: {
    BINDING *b = e->e.bnd;
    DECL *d;
    assert (b);
    d = b->bind_decl;
    assert (d);
    e->expr_obj = get_decl_object (d);
    break;
  }
  case ExprCall_: {
    EXPRCALL *c = e->e.call;
    assert (c);
    alloc_fold_exprlist (c->call_actuals);
    break;
  }
  default:
    break;
  }
}

static int max (int a, int b)
{
  return ((a > b) ? a : b);
}

void needs_expr (EXPR *e)
{
  /* ESTIMATES actual number of temporaries or registers required */
  /* Actual allocation may differ if it is hard to predict, etc.  */
  assert (e);
  switch (e->tag) {
  case ExprBinary_: {
    EXPRBINARY *b = e->e.bin;
    EXPR *el, *er;
    assert (b);
    el = b->bin_left;
    er = b->bin_right;
    needs_expr (el);
    needs_expr (er);
    e->reg_count
      = max (el->reg_count, er->reg_count) + (el->reg_count == er->reg_count);
    break;
  }
  case ExprUnary_: {
    EXPRUNARY *u = e->e.un;
    EXPR *eu;
    assert (u);
    eu = u->un_expr;
    needs_expr (eu);
    e->reg_count = eu->reg_count;
    break;
  }
  case ExprCall_: {
    EXPRCALL *c = e->e.call;
    BINDING *b;
    DECL *d;
    EXPRLIST *actuals;
    assert (c);
    b = c->call_name;
    assert (b);
    d = b->bind_decl;
    assert (d);
    e->reg_count = (e->expr_type ? ALLOC_REGS : 0);
    for (actuals = c->call_actuals; actuals; actuals = actuals->rest)
      needs_expr (actuals->this);
    break;
  }
  case ExprText_:
    e->reg_count = 1;
    break;
  case ExprBind_: {
    BINDING *b = e->e.bnd;
    DECL *d;
    OBJECT *o;
    assert (b);
    d = b->bind_decl;
    assert (d);
    o = get_decl_object (d);
    assert (o);
    if (o->tag == ObjValue_)
      e->reg_count = 0; /* assume does not need register, or that the */
                        /* assembler will use $at, so we need not give */
                        /* some other register */
    else {
      MEMORY *m = o->o.mem;
      assert (m);
      e->reg_count = is_non_addr_expr (e);
      /* requires a register if on rhs and not an address */
    }
    break;
  }
  default:
    e->reg_count = 0;
    break;
  }
  return;
}

void alloc_temp_and_reg (EXPR *e)
{
  if (e->expr_obj == 0) {
    int indirect = 0;
    if (e->tag == ExprUnary_) {
      EXPRUNARY *u = e->e.un;
      assert (u);
      indirect = (u->un_op == '^') && !is_non_addr_expr (e);
    } else if (e->tag == ExprBinary_) {
      EXPRBINARY *b = e->e.bin;
      assert (b);
      indirect = (b->bin_op == '[' || b->bin_op == '.')
	&& !is_non_addr_expr (e);
    }
    e->expr_obj = alloc_temp (e->expr_type, indirect);
  }
  {
    OBJECT *o = e->expr_obj;
    assert (o);
    if (o->tag == ObjMem_) {
      /* assume that constants do not need a register */
      /* eligibility for register allocation is as follows: */
      /* - must be of size 4 (fundamentally, or because it is indirect) */
      /* - on rhs or an address expression */
      MEMORY *m = o->o.mem;
      assert (m);
      if (temp_size (e->expr_type, m->mem_indirect) == 4 &&
	  (e->expr_rhs || m->mem_indirect)) {
	/* eligible for register allocation */
	alloc_reg (e);
      }
    }
  }
}

void free_temp_and_reg (EXPR *e)
{
  free_temp (e->expr_obj);
  free_reg  (e);
}

void spill_expr (EXPR *e)
{
  /* force a spill and free any register now, before calculating */
  /* siblings, etc.                                              */
  free_reg (e);
  e->reg_spill = 1;
}

void alloc_expr (EXPR *e, int will_spill)
{
  assert (e);
  switch (e->tag) {
  case ExprBinary_: {
    EXPRBINARY *b = e->e.bin;
    EXPR *el, *er;
    int op, lfirst, spill;
    assert (b);
    el = b->bin_left;
    er = b->bin_right;
    op = b->bin_op;
    assert (el && er);
    lfirst = (el->reg_count >= er->reg_count);
    spill = (lfirst ? er : el)->reg_count >= ALLOC_REGS;
    alloc_expr ((lfirst ? el : er), spill);
    if (regs_available == 0 || spill)
      spill_expr (lfirst ? el : er);
    alloc_expr ((lfirst ? er : el), 0);
    (lfirst ? er : el)->expr_later = 1;
    switch (op) {
    case '[':
    case '.':
      if (e->expr_obj == 0) {
	if ((op == '.' || get_constant_value(er)) &&
	    will_spill == 0 & el->reg_spill != 1) {
	  /* we can do partial folding of address arithmetic */
	  OBJECT *ol = el->expr_obj;
	  MEMORY *ml;
	  int offset, increment, reg;
	  assert (ol && ol->tag == ObjMem_);
	  ml = ol->o.mem;
	  assert (ml);
	  offset = (ml->mem_indirect ? 0 : ml->mem_offset);
	  increment = (op == '[' ? get_element_offset (el, er) :
		       get_field_offset   (el, er));
	  if (increment >= 0) {
	    reg = el->reg_name;

	    /* can simply offset from the register used before for el */
	    e->expr_obj = make_mem_object (e->expr_type, 0, 0,
					   1000 + reg,
					   offset + increment);
	    free_temp (lfirst ? er->expr_obj : el->expr_obj);
	    free_temp (lfirst ? el->expr_obj : er->expr_obj);
	    free_reg (er);

	    /* We must not free reg until e is ready to be freed; yet e will */
	    /* not always use reg to hold the value of e. We mark that case  */
	    /* with reg_spill = 2, but note that it is different from a      */
	    /* regular spill, and can also be told apart by the memory level */
	    /* being >= 1000. */

	    e->reg_name = reg;
	    if (!(is_non_addr_expr (e) && e->expr_type->size == 4))
	      e->reg_spill = 2;
	    break;		/* drop out of case after default */
	  }
	  warning (er->lineno, "Warning: constant index out of range");
	}
      }
      /* fall through to default */
    default:
      /* free order does not really matter, but the allocation list keeps */
      /* a better order if we free in reverse order of allocation */
      free_temp_and_reg (lfirst ? er : el);
      free_temp_and_reg (lfirst ? el : er);
      alloc_temp_and_reg (e);
      break;
    }
    break;
  }
  case ExprUnary_: {
    EXPRUNARY *u = e->e.un;
    EXPR *eu;
    assert (u);
    eu = u->un_expr;
    alloc_expr (eu, 0);
    free_temp_and_reg (eu);
    alloc_temp_and_reg (e);
    break;
  }
  case ExprCall_: {
    EXPRCALL *c = e->e.call;
    BINDING *b;
    DECL *d;
    assert (c);
    b = c->call_name;
    assert (b);
    d = b->bind_decl;
    assert (d);
    if (d->tag == DeclProcFunc_) {
      PROCFUNCDEF *p = d->d.proc;
      assert (p);
      if (p->proc_result_type)
	alloc_temp_and_reg (e);
      /* note: actuals have already been processed! */
    }
    else {
      assert (d->tag == DeclSpecial_);
      /* again, the actuals have already been processed! */
    }
    break;
  }
  case ExprInt_:
  case ExprText_:
  case ExprNil_:
  case ExprBind_:
    alloc_temp_and_reg (e);
    break;
  default:
    break;
  }
  return;
}
