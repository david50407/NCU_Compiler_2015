# include "project.h"

/**********************************************************************/
/*                       Some well known types                        */
/**********************************************************************/

TYPE *the_bool_type = 0;  /* Note: these are set up by init_types */
TYPE *the_err_type  = 0;
TYPE *the_int_type  = 0;
TYPE *the_txt_type  = 0;
TYPE *the_nil_type  = 0;

/**********************************************************************/
/*                    Type examination functions                      */
/**********************************************************************/

int is_bool_type (TYPE *t)
{
  return (t == the_bool_type);
}

int is_err_type (TYPE *t)
{
  return (t == 0 || t->tag == TypeErr_);
}

int is_enum_type (TYPE *t)
{
  return (t && t->tag == TypeEnum_);
}

int is_ptr_type (TYPE *t)
{
  return (t && (t->tag == TypePointer_ || t->tag == TypeNil_));
}

int is_int_type (TYPE *t)
{
  return (t == the_int_type);
}

int is_txt_type (TYPE *t)
{
  return (t == the_txt_type);
}

int is_nil_type (TYPE *t)
{
  return (t == the_nil_type);
}

TYPE *base_type_of (TYPE *t)
{
  return ((t && t->tag == TypeSubrange_) ? t->t.rng->rng_base : t);
}

int is_int_based_type (TYPE *t)
{
  return (base_type_of (t) == the_int_type);
}

int is_ordinal_type (TYPE *t)
{
  if (t)
    switch (t->tag) {
    case TypeInt_:
    case TypeSubrange_:
    case TypeEnum_:
    case TypeBool_:
      return 1;
    default:
      break;
    }
  return 0;
}

int index_lo_num (TYPE *t)
{
  assert (t);
  switch (t->tag) {
  case TypeSubrange_:
    return subrange_lo_num (t);
  default:
    return 0;
  }
}

int index_hi_num (TYPE *t)
{
  assert (t);
  switch (t->tag) {
  case TypeSubrange_:
    return subrange_hi_num (t);
  case TypeBool_:
    return 1;
  case TypeEnum_:
    return enum_count (t->t.enu);
  default:
    return -1;
  }
}

int subrange_lo_num (TYPE *t)
{
  TYPESUBR *s;
  EXPR *e;
  assert (t && t->tag == TypeSubrange_);
  s = t->t.rng;
  assert (s);
  e = s->rng_lo;
  assert (e);
  return get_const_num (e);
}

int subrange_hi_num (TYPE *t)
{
  TYPESUBR *s;
  EXPR *e;
  assert (t && t->tag == TypeSubrange_);
  s = t->t.rng;
  assert (s);
  e = s->rng_hi;
  assert (e);
  return get_const_num (e);
}

int enum_count (BINDLIST *bl)
{
  unsigned i = 0;
  for ( ; bl; bl = bl->rest) ++i;
  return i;
}
        
/**********************************************************************/
/*                     Type comparing functions                       */
/**********************************************************************/

static int same_pointer_type (TYPE *t1, TYPE *t2)
{
  int ok = (t1 && t2 && t1->tag == TypePointer_ && t2->tag == TypePointer_);
  if (ok) {
    TYPE *pt1 = t1->t.ptr, *pt2 = t2->t.ptr;
    BINDING *b1, *b2;
    assert (pt1 && pt2 && pt1->tag == TypeIdent_ && pt2->tag == TypeIdent_);
    b1 = pt1->t.bind;
    b2 = pt2->t.bind;
    assert (b1 && b2);
    return (b1->bind_decl == b2->bind_decl);
  }
  return 0;
}

int are_compatible_types (TYPE *t1, TYPE *t2)
{
  if (t1 && t2) {
    TYPEKIND tag1 = t1->tag;
    TYPEKIND tag2 = t2->tag;
    return ((t1 == t2) ||
	    (base_type_of (t1) == base_type_of (t2)) ||
	    ((tag1 == TypePointer_) && (tag2 == TypeNil_    )) ||
	    ((tag1 == TypeNil_    ) && (tag2 == TypePointer_)) ||
	    same_pointer_type (t1, t2));
  }
  return 0;
}

int are_assignable_types (TYPE *t1, TYPE *t2)
{
  /* NOTE: t1 is the one being assigned to, t2 the one being assigned; */
  /* for now, assignment compatibility and ordinary compatibility are the same */
  return are_compatible_types (t1, t2);
}

/**********************************************************************/
/*                      Resolving functions                           */
/**********************************************************************/

void resolve_type (TYPE **pt)
{
  TYPE *t = *pt;
  assert (t);
  switch (t->tag) {
  case TypeEnum_: {
    BINDLIST *bl;
    int count = 0;

    assert (t && t->tag == TypeEnum_);
    assert (t->t.enu);
    for (bl = t->t.enu; bl; bl = bl->rest) {
      BINDING *b = bl->this;
      EXPR *e = make_int_expr (count++);

      e->expr_type = t;
      e->lineno = b->lineno;
      b->bind_decl = make_const_decl (b->bind_id, e);
      b->bind_decl->lineno = b->lineno;
      current_symtab->symtab_decls
	= make_decllist (b->bind_decl, current_symtab->symtab_decls);
      enter_decl (b->bind_decl);
    }
    break;
  }
  case TypeSubrange_: {
    TYPESUBR *s = t->t.rng;
    assert (s->rng_lo && s->rng_hi);
    bind_const (s->rng_lo);
    bind_const (s->rng_hi);
    break;
  }
  case TypeIdent_: {
    DECL *d = check_bind_use (t->t.bind);
    if (d == 0)
      *pt = the_err_type;
    else if (d->tag == DeclType_)
      *pt = d->d.typ;
    else {
      errmsg (t->lineno, "%s is not bound to a type", d->decl_id->id_chars);
      *pt = the_err_type;
    }
    break;
  }
  case TypeArray_: {
    TYPEARRAY *a = t->t.arr;
    assert (a && a->arr_index && a->arr_element);
    resolve_type (&(a->arr_index));
    resolve_type (&(a->arr_element));
    break;
  }
  case TypeRecord_: {
    FIELDLIST *fl = t->t.rec;
    for (; fl; fl = fl->rest)
      resolve_type (&(fl->this->field_type));
    break;
  }
  case TypePointer_:
    /* Pointer types are specifically deferred */
  case TypeText_ :
  case TypeInt_ :
  case TypeNil_ :
  case TypeBool_:
  case TypeErr_ :
    break;
  }
}

/**********************************************************************/
/*                      Checking functions                            */
/**********************************************************************/

int check_type (TYPE **pt)
{
  /* Note: returns an ok indicator AND mutates t as necessary */

  TYPE *t = *pt;
  int ok = 1;
  switch (t->tag) {
  case TypeSubrange_: {
    TYPESUBR *s = t->t.rng;
    EXPR *lo;
    EXPR *hi;
    assert (s);
    ok = check_const (lo = s->rng_lo) & check_const (hi = s->rng_hi);
    if (ok && !(ok = are_compatible_types (lo->expr_type, hi->expr_type)))
      errmsg (t->lineno, "Range bounds have incompatible types");
    if (ok && !(ok = is_ordinal_type (lo->expr_type)))
      errmsg (t->lineno, "Range types must be based on integers");
    if (ok && !(ok = (subrange_lo_num (t) <= subrange_hi_num (t))))
      errmsg (t->lineno, "Range lower limit exceeds upper limit");
    if (ok)
      s->rng_base = base_type_of (lo->expr_type);
    break;
  }
  case TypeArray_: {
    TYPEARRAY *a = t->t.arr;
    TYPE *idx;
    unsigned number;
    assert (a && a->arr_index && a->arr_element);
    ok = check_type (&(a->arr_index));
    if (ok && !(ok = is_ordinal_type (idx = a->arr_index)))
      errmsg (t->lineno, "Index types must be ordinal types");
    if (ok && !(ok = !is_int_type (idx)))
      errmsg (t->lineno, "INTEGER may not be an index type");
    if (ok && idx->tag == TypeSubrange_) {
      number = subrange_hi_num (idx) - subrange_lo_num (idx) + 1;
      if (number > 1000000)
	warning (t->lineno, "Warning: array type very large");
    }
    if (ok && idx->tag == TypeEnum_)
      number = enum_count (idx->t.enu);
    if (ok && idx->tag == TypeBool_)
      number = 2;
    if (ok &= check_type (&(a->arr_element)))
      t->size = number * a->arr_element->size;
    break;
  }
  case TypeRecord_: {
    FIELDLIST *fl = t->t.rec;
    unsigned offset = 0;
    for (; fl; fl = fl->rest) {
      FIELD *f = fl->this;
      if (ok &= check_type (&(f->field_type))) {
        f->field_offset = offset;
        offset += f->field_type->size;
      }
      if (find_field (fl->rest, f->field_id)) {
	errmsg (t->lineno, "Duplicate field name %s", f->field_id->id_chars);
	ok = 0;
      }
    }
    t->size = offset;
    if (offset > MAX_FRAME_SIZE)
      warning (t->lineno, "Warning: record type very large");       
    break;
  }
  case TypePointer_: {
    /* check the name if it has not been done already */
    TYPE *p = t->t.ptr;
    assert (p);

    /* Note: we pass p so that we do NOT bypass the binding here */
    ok = ((p->tag == TypeIdent_) && check_type (&p));
    break;
  }
  case TypeIdent_: {
    BINDING *b = t->t.bind;
    DECL *d;
    assert (b);
    ok = 0;
    if ((d = check_bind_use (b))) {
      if (d->tag != DeclType_)
	errmsg (t->lineno, "%s is not a type", b->bind_id->id_chars);
      else {
	/* bypass the binding */
	*pt = d->d.typ;
	ok = 1;
      }
    }
    break;
  }
  case TypeEnum_:
  case TypeText_:
  case TypeInt_ :
  case TypeNil_ :
  case TypeBool_:
  case TypeErr_ :
    break;
 default:
    errmsg (t->lineno, "Unimplemented kind of type");
    ok = 0;
    break;
  }

  if (!ok)
    *pt = the_err_type;
  return ok;
}

/**********************************************************************/
/*                      Miscellaneous functions                       */
/**********************************************************************/

FIELD *find_field (FIELDLIST *fl, IDENT *id)
{
  for ( ; fl; fl = fl->rest) {
    FIELD *f = fl->this;
    if (f->field_id == id)
      return f;
  }
  return 0;
}

void init_types (void)
{
  the_bool_type = make_type (TypeBool_);
  the_err_type  = make_type (TypeErr_ );
  the_int_type  = make_type (TypeInt_ );
  the_txt_type  = make_type (TypeText_);
  the_nil_type  = make_type (TypeNil_ );
  the_int_type ->size = 4;
  the_bool_type->size = 4;
  the_nil_type ->size = 4;
}
