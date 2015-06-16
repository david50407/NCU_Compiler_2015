/* VALUE routines */
# include "project.h"

VALUE *make_value (VALUEKIND vk)
{
  VALUE *v = anew (VALUE);
  v->tag = vk;
  return v;
}

VALUE *make_int_value (int i)
{
  VALUE *v = make_value (ValueInt_);
  v->v.intx = i;
  return v;
}

VALUE *make_txt_value (TEXT *t)
{
  VALUE *v = make_value (ValueStr_);
  v->v.text = t;
  return v;
}

int value_equal (VALUE *v1, VALUE *v2)
{
  if (v1 && v2 && v1->tag == v2->tag) {
    switch (v1->tag) {
    case ValueInt_ : return (v1->v.intx == v2->v.intx);
    case ValueStr_ : return (v1->v.text == v2->v.text);
    default        : break;
    }
  }
  return 0;
}

void value_print (VALUE *v)
/* A print routine for testing and debugging */
{
  switch (v->tag) {
  case ValueInt_:
    printf ("%d", v->v.intx);
    break;
  case ValueStr_:
    printf ("\'%s\'", v->v.text->text_chars);
    break;
  default:
    printf ("Request to print unknown value\n");
    break;
  }
}
