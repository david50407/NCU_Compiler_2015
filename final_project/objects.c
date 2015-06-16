/* OBJECT routines */
# include "project.h"

/* Allocate once at initialization, and use for any errors. */

OBJECT *the_false_object = 0;
OBJECT *the_true_object  = 0;
OBJECT *the_err_object   = 0;
OBJECT *the_nil_object   = 0;

OBJECT *make_object (OBJKIND k)
{
  OBJECT *o = anew (OBJECT);
  o->tag      = k;
  o->obj_type = 0;

  o->o.val = 0;
  return o;
}

OBJECT *make_value_object (TYPE *t, VALUE *v)
{
  OBJECT *o = make_object (ObjValue_);
  o->obj_type = t;
  o->o.val = v;
  return o;
}

OBJECT *make_int_object (int i)
{
  return make_value_object (the_int_type, make_int_value (i));
}

OBJECT *make_txt_object (TEXT *t)
{
  return make_value_object (the_txt_type, make_txt_value (t));
}

MEMORY *make_memory (int temp, int indirect, int plevel, int offset)
{
  MEMORY *m = anew (MEMORY);
  m->mem_plevel   = plevel;
  m->mem_offset   = offset;
  m->mem_temp     = temp;
  m->mem_indirect = indirect;
  return m;
}

OBJECT *make_mem_object (TYPE *t, int temp, int indirect, int plevel,
			 int offset)
{
  OBJECT *o = make_object (ObjMem_);
  o->obj_type = t;
  o->o.mem = make_memory (temp, indirect, plevel, offset);
  return o;
}

void init_objects ()
/* Create error object only once at initialization,
   then re-use for all errors; likewise for false, true, nil
   Note: must be called _after_ init_types.
 */
{
  assert (the_bool_type);
  the_false_object = make_value_object (the_bool_type, make_int_value (0));
  the_true_object  = make_value_object (the_bool_type, make_int_value (1));

  assert (the_nil_type);
  the_nil_object = make_value_object (the_nil_type, make_int_value (0));

  assert (the_err_type);
  the_err_object = make_object (ObjErr_);
  the_err_object->obj_type = the_err_type;
}
