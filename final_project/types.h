/* some well known types */

extern TYPE *the_bool_type;
extern TYPE *the_err_type ;
extern TYPE *the_int_type ;
extern TYPE *the_txt_type ;
extern TYPE *the_nil_type ;

/* examination functions */

extern int is_bool_type (TYPE *t);
extern int is_err_type  (TYPE *t);
extern int is_enum_type (TYPE *t);
extern int is_ptr_type  (TYPE *t);
extern int is_int_type  (TYPE *t);
extern int is_txt_type  (TYPE *t);
extern int is_nil_type  (TYPE *t);

extern TYPE *base_type_of    (TYPE *t);
extern int is_int_based_type (TYPE *t);
extern int is_ordinal_type   (TYPE *t);
extern int index_lo_num      (TYPE *t);
extern int index_hi_num      (TYPE *t);
extern int subrange_lo_num   (TYPE *t);
extern int subrange_hi_num   (TYPE *t);
extern int enum_count (BINDLIST *bl);

/* comparison functions */

extern int are_compatible_types (TYPE *t1, TYPE *t2);
extern int are_assignable_types (TYPE *t1, TYPE *t2);

/* resolving functions */

extern void expand_enum_type     (TYPE  *t );
extern void resolve_type (TYPE **pt);

/* checking functions */

extern int check_type (TYPE **pt);

/* miscellaneous functions */

extern FIELD *find_field (FIELDLIST *fl, IDENT *id);

extern void init_types (void);
