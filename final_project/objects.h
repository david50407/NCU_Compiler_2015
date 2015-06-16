extern OBJECT *the_false_object;
extern OBJECT *the_true_object ;
extern OBJECT *the_err_object  ;
extern OBJECT *the_nil_object  ;

extern OBJECT *make_object       (OBJKIND k);
extern OBJECT *make_value_object (TYPE *t, VALUE *v);

extern OBJECT *make_int_object (int i);
extern OBJECT *make_txt_object (TEXT *t);

extern MEMORY *make_memory     (int temp, int indirect,	int plevel, int offset);
extern OBJECT *make_mem_object (TYPE *t, int temp,
				int indirect, int plevel, int offset);

extern void init_objects (void);
