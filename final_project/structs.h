typedef enum Boolean { FALSE, TRUE } BOOLEAN ;

/**********************************************************************/
/*                         SCANNER STRUCTURES                         */
/**********************************************************************/

/* Structure of entry in identifier table */
typedef struct Ident {
  struct Ident  *id_next;	/* chain of identifiers in same hash bucket */
  struct Decl   *id_curr_decl;	/* current declaration */
  char          *id_chars;	/* null terminated name of identifier */
  char           id_reserved;	/* 0 = ordinary, 1 = reserved word */
} IDENT;

/* Structure of entry in text table */
typedef struct Text {
  struct Text  *text_next;	/* chain of text literals in same bucket */
  char         *text_chars;	/* null-terminated character string */
  int           text_number;	/* unique number of string (for its label) */
} TEXT;

/**********************************************************************/
/*                       SYMBOL TABLE STRUCTURES                      */
/**********************************************************************/

/* Structure of entry in scope stack of symbol table */
typedef struct Symtab {
  struct Symtab    *symtab_outer;  /* next outer scope */
  struct DeclList  *symtab_decls;  /* chain of declarations in this scope */
  int               symtab_number; /* scope number (assigned sequentially) */
  int               symtab_level;  /* nesting level (1 = global, etc.) */
  int               symtab_plevel; /* procedure nesting level */
  int               symtab_count;  /* count of decls in symtab */
  int               symtab_offset; /* offset in frame for next variable */
                                   /* NOTE: offsets are generally negative;  */
                                   /* the exception is arguments/formals     */
} SYMTAB;

/* Structure of a name binding */
typedef struct Binding {
  IDENT        *bind_id;	/* the name being bound */
  struct Decl  *bind_decl;	/* the decl to which it is bound */
  int           lineno;		/* makes it easier for error messages */
} BINDING ;

typedef struct BindList {
  BINDING        *this;
  struct BindList  *rest;
} BINDLIST;

/**********************************************************************/
/*                   ABSTRACT SYNTAX TREE STRUCTURES                  */
/**********************************************************************/

typedef struct Program {
  struct DeclList *prog_decls;   /* (for now) includes only program name */
  struct Block    *prog_block;   /* the block (decls plus main BEGIN/END) */
  struct Symtab   *prog_syminfo; /* info for the program symtab */
} PROGRAM;

typedef struct Block {
  struct DeclList *block_decls;
  struct StmtList *block_stmts;
  struct Symtab   *block_syminfo; /* info for the block symtab */
} BLOCK;

/**********************************************************************/
/*                       EXPRESSION STRUCTURES                        */
/**********************************************************************/

typedef enum ExprKind {
  ExprBinary_,     /* a binary operator expression */
  ExprUnary_,      /* a unary operator expression */
  ExprInt_,        /* an integer literal */
  ExprText_,       /* a string literal */
  ExprNil_,        /* the literal NIL */
  ExprBind_,       /* use of an identifier (var, const, proc/func, ...) */
  ExprIdent_,      /* identifier without binding (as in record selection) */
  ExprCall_,       /* function call */
  ExprErr_
} EXPRKIND;

typedef union ExprUnion {
  struct ExprBinary   *bin;  /* binary */
  struct ExprUnary    *un;   /* unary */
  int                  num;  /* number */
  struct Text         *txt;  /* text */
  struct Binding      *bnd;  /* identifier (with binding) */
  struct Ident        *id;   /* identifier (w/out binding, e.g., field name) */
  struct ExprCall     *call; /* call */
} EXPRUNION;

typedef struct Expr {
  enum  ExprKind   tag;
  union ExprUnion  e;
  int              lineno;
  struct Type     *expr_type;	/* type of the expression */
  struct Object   *expr_obj;	/* OBJECT node for address/value of expr */
  unsigned char    expr_rhs;	/* is this an rvalue? */
  unsigned char    expr_later;	/* eval after sibling(s)? */
  unsigned char    reg_count;	/* the number of registers used by the expr */
  unsigned char    reg_name;	/* register# used by current expr  */
  unsigned char    reg_spill;	/* must this result be stored to memory? */
} EXPR;

typedef struct ExprBinary {
  int           bin_op;
  struct Expr  *bin_left;
  struct Expr  *bin_right;
} EXPRBINARY;

typedef struct ExprUnary {
  int           un_op;
  struct Expr  *un_expr;
} EXPRUNARY;

typedef struct ExprCall {
  struct Binding   *call_name;
  struct ExprList  *call_actuals;
} EXPRCALL;

typedef struct ExprList {
  struct Expr      *this;
  struct ExprList  *rest;
} EXPRLIST;

/**********************************************************************/
/*                        STATEMENT STRUCTURES                        */
/**********************************************************************/

typedef enum StmtKind {
  StmtEmpty_,
  StmtAssign_,
  StmtCompound_,
  StmtCall_,
  StmtIf_,
  StmtCase_,
  StmtWhile_,
  StmtRepeat_,
  StmtFor_
} STMTKIND;

typedef union StmtUnion {
  struct StmtAssign  *asn;	/* assign */
  struct StmtList    *comp;	/* compound */
  struct Expr        *call;	/* procedure call */
  struct StmtIf      *ifx;	/* if */
  struct StmtCase    *cas;	/* case */
  struct StmtWhile   *wh;	/* while */
  struct StmtRepeat  *rpt;	/* repeat */
  struct StmtFor     *forx;	/* for */
} STMTUNION;

typedef struct Stmt {
  enum  StmtKind   tag;
  union StmtUnion  s;
  int              lineno;
} STMT;

typedef struct StmtAssign {
  struct Expr  *asn_var;
  struct Expr  *asn_expr;
} STMTASSIGN;

typedef struct StmtIf {
  struct Expr  *if_expr;
  struct Stmt  *if_then;
  struct Stmt  *if_else;
} STMTIF;

typedef struct StmtCase {
  struct Expr      *cas_expr;
  struct CaseList  *cas_arms;
} STMTCASE;

typedef struct CaseList {
  struct CaseArm   *this;
  struct CaseList  *rest;
} CASELIST;

typedef struct CaseArm {
  struct ExprList  *arm_labels;
  struct Stmt      *arm_stmt;
} CASEARM;

typedef struct StmtWhile {
  struct Expr  *wh_expr;
  struct Stmt  *wh_stmt;
} STMTWHILE;

typedef struct StmtRepeat {
  struct Expr      *rpt_expr;
  struct StmtList  *rpt_stmts;
} STMTREPEAT;

typedef struct StmtFor {
  struct Binding  *for_name;
  struct Expr     *for_init;
  struct Expr     *for_to;
  struct Object   *for_limit_obj;
  struct Stmt     *for_stmt;
  enum Boolean     for_upward;
} STMTFOR;

typedef struct StmtList {
  struct Stmt      *this;
  struct StmtList  *rest;
} STMTLIST;

/**********************************************************************/
/*                       DECLARATION STRUCTURES                       */
/**********************************************************************/

typedef enum DeclKind {
  DeclConst_,     /* for CONST decls */
  DeclType_,      /* for TYPE decls */
  DeclVar_,       /* for VAR decls */
  DeclProcFunc_,  /* for PROCEDURE and FUNCTION decls */
  DeclProgram_,   /* for the program name */
  DeclFormal_,    /* for proc/func formal arguments */
  DeclSpecial_,   /* for the special built-ins (read, write, etc.) */
  DeclErr_        /* a placeholder for bad decls */
} DECLKIND;

/* These simply identify the special built-in functions */
typedef enum SpecialWhich {
  SpecialReadln_,
  SpecialWrite_, SpecialWriteln_,
  SpecialNew_
} SPECIALWHICH;

typedef union DeclUnion {
  struct Expr         *con;	/* Const declaration */
  struct Type         *typ;	/* Type declaration */
  struct VarDef       *var;	/* Var declaration  */
  struct ProcFuncDef  *proc;	/* Procedure or function declaration */
  struct Formal       *frm;	/* formal Arguments */
  enum SpecialWhich    spc;	/* Special: read, write, etc. */
} DECLUNION;

typedef struct Decl {
  enum DeclKind    tag;         /* declaration kind */
  union DeclUnion  d;           /* declaration of the id */
  int              lineno;      /* location, for error messages (?) */
  struct Ident    *decl_id;     /* identifier being declared */
  struct Decl     *decl_outer;  /* next outer declaration of same id */
  struct Symtab   *decl_symtab; /* symbol table of the declaration */
  int              decl_number; /* declaration number within a scope */
} DECL;

typedef struct VarDef {
  struct Type   *var_type ;
  struct Object *var_addr ;
} VARDEF ;

typedef struct ProcFuncDef {
  struct DeclList  *proc_formals;     /* list of formals */
  struct Type      *proc_result_type; /* null for a proc, type for a func */
  struct Block     *proc_block;       /* the local decls and body */
  struct Symtab    *proc_syminfo;     /* info for proc/func symtab */
  int               proc_formals_result_size;
                      /* total size for formal param & return type */ 
  struct Object    *proc_result_addr; /* func result object (0 for proc) */
  int               proc_top_label;   /* the top label of the proc/func */
} PROCFUNCDEF;

typedef enum ModeWhich { ModeValue_ , ModeVar_ } MODEWHICH;

typedef struct Formal {
  enum ModeWhich  frm_mode ;
  struct Type    *frm_type ;
  struct Object  *frm_addr ;
} FORMAL;

typedef struct DeclList {
  struct Decl     *this;
  struct DeclList *rest;
} DECLLIST;

/**********************************************************************/
/*                     TYPE DESCRIPTOR STRUCTURES                     */
/**********************************************************************/

typedef enum TypeKind {
  TypeEnum_,      /* enumeration types */
  TypeSubrange_,  /* subrange types (of integer or enumerations) */
  TypeArray_,     /* array types */
  TypeRecord_,    /* record types */
  TypePointer_,   /* pointer types */
  TypeInt_,       /* the integer type */
  TypeBool_,      /* the boolean type (not treated as an enumeration) */
  TypeText_,      /* the type of string constants */
  TypeNil_,       /* special marker for NIL, which matches any pointer type */
  TypeIdent_,     /* for synonym type declarations */
  TypeErr_        /* placeholder for bad types */
} TYPEKIND;

typedef union TypeUnion {
  struct BindList   *enu;	/* enumeration type info */
  struct TypeSubr   *rng;	/* subrange type info */
  struct Binding    *bind;	/* type identifier (synonym binding) */
  struct TypeArray  *arr;	/* array type info */
  struct FieldList  *rec;	/* record type info */
  struct Type       *ptr;	/* pointer type info */
} TYPEUNION;

typedef struct Type {
  enum TypeKind    tag;
  union TypeUnion  t;
  int              lineno;	/* for error messages */
  int              size;
} TYPE;

typedef struct TypeSubr {
  struct Type  *rng_base;	/* what the range is based on (int or enum) */
  struct Expr  *rng_lo;
  struct Expr  *rng_hi;
} TYPESUBR;

typedef struct TypeArray {
  struct Type  *arr_index;	/* the index type (must be range or enum) */
  struct Type  *arr_element;	/* the element type */
} TYPEARRAY;

typedef struct Field {
  struct Ident  *field_id;
  struct Type   *field_type;
  int            field_offset;
} FIELD;

typedef struct FieldList {
  struct Field      *this;
  struct FieldList  *rest;
} FIELDLIST;

/**********************************************************************/
/*                    OBJECT DESCRIPTOR STRUCTURES                    */
/*                                                                    */
/* Objects describe where variables and values can be found at run    */
/* time. They encompass known values (literals, user declared         */
/* constants, constants resulting from constant folding, etc.) and    */
/* locations of computed quantities (variables, formals/arguments,    */
/* and compiler temporaries, including ones used for address          */
/* arithmetic).                                                       */
/**********************************************************************/

typedef enum ObjKind { ObjValue_, ObjMem_, ObjErr_ } OBJKIND;

typedef union ObjUnion {
  struct Value   *val;
  struct Memory  *mem;
} OBJUNION;

typedef struct Object {
  enum ObjKind    tag;
  union ObjUnion  o;
  struct Type    *obj_type;
} OBJECT;

typedef struct Memory {
  int      mem_plevel;   /* procedure level */
  int      mem_offset;   /* offset from frame pointer at that level */
  BOOLEAN  mem_indirect; /* true/false; true means the indicated place */
                         /* contains the (4 byte) _address_ of the object */
                         /* rather than the object itself */
  int      mem_temp;	 /* 0 = not a temporary */
                         /* 1-99 = number of uses until freed automatically */
                         /* 100 = will be freed explicitly */
} MEMORY;

/**********************************************************************/
/*                    VALUE DESCRIPTOR STRUCTURES                     */
/*                                                                    */
/* Values describe literals, user declared constants, or compiler     */
/* computed constants, i.e., all quantities known at compile time.    */
/**********************************************************************/

typedef enum ValueKind { ValueInt_, ValueStr_, ValueErr_ } VALUEKIND;

typedef union ValueUnion {
  int           intx;
  struct Text  *text;
} VALUEUNION;

typedef struct Value {
  enum ValueKind    tag;
  union ValueUnion  v;
} VALUE;

/**********************************************************************/
/*                  General Declarations and Macros                   */
/**********************************************************************/

# define anew(T) (T *)(malloc (sizeof (T)))
