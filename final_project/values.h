extern VALUE *make_value (VALUEKIND vk);

extern VALUE *make_int_value (int i);
extern VALUE *make_txt_value (TEXT *t);

extern int value_equal (VALUE *v1, VALUE *v2);

extern void value_print (VALUE *v);
