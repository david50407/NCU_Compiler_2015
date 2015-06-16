# include "project.h"
# include "parse.tab.h"

/* a couple of useful helper functions, to assist in finding unneeded */
/* bounds and range checks */

static int min_value (EXPR *e)
{
  TYPE *t;
  assert (e);
  t = e->expr_type;
  assert (t);
  return (t->tag == TypeInt_) ? 0x80000000 : index_lo_num (t);
}

static int max_value (EXPR *e)
{
  TYPE *t;
  assert (e);
  t = e->expr_type;
  assert (t);
  return (t->tag == TypeInt_) ? 0x7fffffff : index_hi_num (t);
}

/* this variable is needed for determining levels of static */
/* chain to follow when accessing variables, etc. */
int current_plevel;

void gen_program (PROGRAM *p)
{ 
  if (p == 0)
    return;
  current_plevel = p->prog_syminfo->symtab_plevel;
  gen_program_start (p);
  gen_block_body  (p->prog_block); 
  gen_program_block_end (p);
  gen_block_procs (p->prog_block);
  gen_program_end (p);
}

void gen_program_start (PROGRAM *p)
{
  int offset = p->prog_block->block_syminfo->symtab_offset;

  /* .text ensures we are generating words into the instruction area */
  gen ("\t.text\n");

  /* .globl ensures that "main" is seen by the rest of the run-time  */
  /* main: is the entry point to the program */
  gen ("\t.globl main\n");
  gen ("main:\n");

  /* initialize the stack pointer to refer to high end of the stack area */
  /* make room for main's linkage */
  gen_inst ("sub % % #", REG_SP, REG_SP, LINKAGE_SIZE);

  /* REG_SL will point to the low end of the stack area, for stack overflow */
  /* checking. It is a shorthand for $s7 */
  gen_inst ("sub % % #"    , REG_SL, REG_SP, 8192);

  /* save the return address and dynamic chain; the main has no real static link */
  gen_inst ("sw % #(%)"    , REG_RA, RETURN_ADDR, REG_SP);
  gen_inst ("sw % #(%)"    , REG_FP, DYNAMIC_CHAIN, REG_SP);
  gen_inst ("sw % #(%)"    , REG_0, STATIC_CHAIN, REG_SP);

  /* set up the frame pointer */
  gen_inst ("move % %"     , REG_FP, REG_SP);

  /* allocate room for locals of main (globals of the program) */
  if (offset) {
    gen_inst ("sub % % #", REG_SP, REG_SP, -offset);

    /* check for stack overflow */
    gen_inst ("ble % % STKOV", REG_SP, REG_SL);
  }
}

void gen_program_block_end (PROGRAM *p)
{
  /* set up exit code in $v0 as required by calling protocol */
  gen_inst ("li % 0", REG_V0);    /* need to return exit code of 0 */

  /* deallocate frame */
  gen_inst ("move % %", REG_SP, REG_FP);

  /* restore frame pointer */
  gen_inst ("lw % #(%)", REG_FP, DYNAMIC_CHAIN, REG_SP);

  /* get return address into $ra */
  gen_inst ("lw % #(%)", REG_RA, RETURN_ADDR, REG_SP);

  /* deallocate main's linkage area */
  gen_inst ("add % % #", REG_SP, REG_SP, LINKAGE_SIZE);
  gen_inst ("jr %"     , REG_RA);
}
 
void gen_program_end (PROGRAM *p)
{
  /* set up a variety of different breaks to indicate run-time failure */
  gen ("BADRNG:\n");
  gen_inst ("la % badrngstr", REG_A0);
  gen_inst ("b failure");

  gen ("BADDIV:\n");
  gen_inst ("la % baddivstr", REG_A0);
  gen_inst ("b failure");

  gen ("BADCAS:\n");
  gen_inst ("la % badcasstr", REG_A0);
  gen_inst ("b failure");

  gen ("BADPTR:\n");
  gen_inst ("la % badptrstr", REG_A0);
  gen_inst ("b failure");

  gen ("BADSUB:\n");
  gen_inst ("la % badsubstr", REG_A0);
  gen_inst ("b failure");

  gen ("STKOV:\n");
  gen_inst ("la % stkovstr", REG_A0);
  gen_inst ("b failure");

  gen ("HEAPOV:\n");
  gen_inst ("la % heapovstr", REG_A0);
  gen_inst ("b failure");

  gen ("failure:\n");
  gen_inst ("jal wstr");
  gen_inst ("li % 10", REG_V0);  /* terminate execution */
  gen_inst ("syscall");

  /* emit run-time procedures that support specials */

  gen ("wstr:\n");
  gen_inst ("li % 4", REG_V0);
  gen_inst ("syscall");
  gen_inst ("jr %", REG_RA);

  gen ("wint:\n");
  gen_inst ("li % 1", REG_V0);
  gen_inst ("syscall");
  gen_inst ("jr %", REG_RA);

  gen ("wln:\n");
  gen_inst ("li % 4", REG_V0);
  gen_inst ("la % nlstr", REG_A0);
  gen_inst ("syscall");
  gen_inst ("jr %", REG_RA);

  gen ("copy:\n");
  gen_inst ("lb % 0(%)", REG_A3, REG_A0);
  gen_inst ("sb % 0(%)", REG_A3, REG_A1);
  gen_inst ("add % % 1", REG_A0, REG_A0);
  gen_inst ("add % % 1", REG_A1, REG_A1);
  gen_inst ("sub % % 1", REG_A2, REG_A2);
  gen_inst ("bgtz % copy", REG_A2);
  gen_inst ("jr %", REG_RA);

  gen ("alloc:\n");
  gen_inst ("lw % heapptr", REG_A1);
  gen_inst ("sub % % %", REG_A0, REG_A1, REG_A0);
  gen_inst ("sw % heapptr", REG_A0);
  gen_inst ("la % heap", REG_A1);
  gen_inst ("blt % % HEAPOV", REG_A0, REG_A1);
  gen_inst ("jr %", REG_RA);

  /* emit text constants, in data area */
  gen ("\t.data\n");
  gen ("nlstr:     .asciiz \"\\n\"\n");
  gen ("badrngstr: .asciiz \"\\nValue out of range in assignment\\n\"\n");
  gen ("baddivstr: .asciiz \"\\nDivision by zero\\n\"\n");
  gen ("badcasstr: .asciiz \"\\nValue not handled in case statement\\n\"\n");
  gen ("badptrstr: .asciiz \"\\nAttempt to use a null pointer\\n\"\n");
  gen ("badsubstr: .asciiz \"\\nSubscript out of bounds\\n\"\n");
  gen ("stkovstr:  .asciiz \"\\nStack overflow\\n\"\n");
  gen ("heapovstr: .asciiz \"\\nOut of heap space\\n\"\n");

  text_iterate (gen_text_constant);

  /* align to the next multiple of 2^2 (i.e., 4) bytes */
  /* give space for the heap */
  gen ("          .align 2\n");
  gen ("heap:     .space 8192\n");
  gen ("heapptr:  .word heapptr\n");
}
 
void gen_block_body (BLOCK *b)
{
  int old_plevel = current_plevel;
  current_plevel = b->block_syminfo->symtab_plevel;
  gen_stmtlist (b->block_stmts);
  current_plevel = old_plevel;
}

void gen_block_procs (BLOCK *b)
{
  int old_plevel = current_plevel;
  DECLLIST *decls;
  current_plevel = b->block_syminfo->symtab_plevel;
  for (decls = b->block_decls; decls; decls = decls->rest) {
    DECL *decl = decls->this;
    if (decl && decl->tag == DeclProcFunc_)
      gen_procfunc (decl->d.proc);
  }
  current_plevel = old_plevel;
}

void gen_proc_start (PROCFUNCDEF *p)
{
  int offset = p->proc_block->block_syminfo->symtab_offset;
  gen_label (p->proc_top_label);
  gen_inst ("sw % #(%)", REG_RA, RETURN_ADDR, REG_SP);
  gen_inst ("sw % #(%)", REG_FP, DYNAMIC_CHAIN, REG_SP);
  gen_inst ("move % %", REG_FP, REG_SP);
  if (offset) {
    gen_inst ("sub % % #", REG_SP, REG_SP, -offset);
    gen_inst ("ble % % STKOV", REG_SP, REG_SL);
  }
}

void gen_proc_end (PROCFUNCDEF *p)
{
  gen_inst ("move % %", REG_SP, REG_FP);
  gen_inst ("lw % #(%)", REG_FP, DYNAMIC_CHAIN, REG_SP);
  gen_inst ("lw % #(%)", REG_RA, RETURN_ADDR, REG_SP);
  gen_inst ("jr %", REG_RA);
}

void gen_procfunc (PROCFUNCDEF *p)
{
  int old_plevel = current_plevel;
  current_plevel = p->proc_syminfo->symtab_plevel;
  gen_proc_start (p);
  gen_block_body  (p->proc_block); 
  gen_proc_end (p);
  gen_block_procs (p->proc_block);
  current_plevel = old_plevel;
}

void gen_stmtlist (STMTLIST *sl)
{
  for ( ; sl; sl = sl->rest)
    if (sl->this)
      gen_stmt (sl->this);
}

void gen_assign (EXPR *te, EXPR *se)
{
  OBJECT *to = te->expr_obj;
  TYPE *tte = te->expr_type;
  assert (to && tte);
  /* assign source expression to target object */
  /* assumes source and target expressions already generated */
  if (tte->tag == TypeSubrange_) {
    /* do range check before assignment */
    int lower = subrange_lo_num (to->obj_type);
    int upper = subrange_hi_num (to->obj_type);
    int sreg = get_expr_in_reg (se, REG_T9);

    if (get_constant_value (se)) {
      int val = get_const_num (se);
      if (lower > val || val > upper) {
	warning (te->lineno, "Warning: constant value out of range");
	gen_inst ("b BADRNG");
	return;
      }
    }
    else {
      if (min_value (se) < lower) {
	gen_inst ("sge % % #", REG_T8, sreg, lower);
	gen_inst ("beqz % BADRNG", REG_T8);
      }
      if (max_value (se) > upper) {
	gen_inst ("sle % % #", REG_T8, sreg, upper);
	gen_inst ("beqz % BADRNG", REG_T8);
      }
    }

    /* now store the value */
    gen_store (sreg, REG_T8, te);
    return;
  }
  if (get_constant_value (se) || tte->size == 4) {
    int sreg = get_expr_in_reg (se, REG_T9);
    gen_store (sreg, REG_T8, te);
    return;
  }
  if (tte->size > 0) {
    int sreg, treg;
    sreg = get_addr_in_reg (se, REG_A0);
    if (sreg != REG_A0)
      gen_inst ("move % %", REG_A0, sreg);
    treg = get_addr_in_reg (te, REG_A1);
    if (treg != REG_A1)
      gen_inst ("move % %", REG_A1, treg);
    gen_inst ("li % #", REG_A2, tte->size);
    gen_inst ("jal copy");
    return;
  }
  assert (0);
}

void do_branch (EXPR *e, int ontrue, int label)
/* generates an appropriate branch based on the comparison operator and */
/*   the ontrue argument */
{
  OBJECT *o;
  if (get_constant_value (e)) {
    if (get_const_num (e) == ontrue) {
      /* generate jump instruction, otherwise generate nothing */
      gen_inst ("b @", label);
    }
    return; 
  }
  o = e->expr_obj;
  assert (o && o->tag == ObjMem_);
  {
    int ereg = get_expr_in_reg (e, REG_T9);
    gen_inst ("~ % @", (ontrue ? "bnez" : "beqz"), ereg, label);
    return;
  } 
}

void gen_stmt_assign (STMTASSIGN *sa)
{
  EXPR *target = sa->asn_var;
  EXPR *source = sa->asn_expr;
  
  if (target->expr_type == the_err_type ||
      source->expr_type == the_err_type )
    return;
  if (source->expr_later) {
    gen_expr (target);
    gen_expr (source);
  }
  else {
    gen_expr (source);
    gen_expr (target);
  }
  gen_assign (target, source);
}

void gen_stmt_if (STMTIF *si)
{
  EXPR *cond  = si->if_expr;
  STMT *thenx = si->if_then;
  STMT *elsex = si->if_else;
  int label1 = choose_label ();
  
  gen_expr (cond);
  do_branch (cond, 0, label1);
  gen_stmt (thenx);
  if (elsex) {
    int label2 = choose_label ();
    gen_inst ("b @", label2);
    gen_label (label1); 
    gen_stmt (elsex);
    gen_label (label2);
  }
  else {
    gen_label (label1);
  }
} 

typedef struct cas_rec {
  EXPR           *expr;
  int             label;
  struct cas_rec *next;
} CAS_REC;

void gen_stmt_case (STMTCASE *sc)
{
  EXPR *e = sc->cas_expr;
  CASELIST *list;
  int label_out   = choose_label ();
  int label_table = choose_label ();
  CAS_REC *crlist = 0;
  int ereg;
  
  /* evaluate the expression and then branch to the dispatching code */
  gen_expr (e);
  ereg = get_expr_in_reg (e, REG_T9);
  gen_inst ("b @", label_table);

  for (list = sc->cas_arms; list; list = list->rest) {
    CASEARM *ca = list->this;  
    int label_case = choose_label ();
    EXPRLIST *values;
    
    /* build a list of the values to associate with this case label */
    for (values = ca->arm_labels; values; values = values->rest) {  
      CAS_REC *cr = anew (CAS_REC);
      cr->expr  = values->this;
      cr->label = label_case;
      cr->next  = crlist;
      crlist = cr;
    }

    /* generate code for this arm */
    gen_label (label_case);
    gen_stmt  (ca->arm_stmt);
    gen_inst  ("b @", label_out);
  }

  /* generate tests and branches */
  gen_label (label_table);
  for ( ; crlist; crlist = crlist->next) {
    int creg = get_expr_in_reg (crlist->expr, REG_T8);
    gen_inst ("beq % % @", ereg, creg, crlist->label);
  }
  gen_inst ("b BADCAS");
  gen_label (label_out);
  return;
}

void gen_stmt_while (STMTWHILE *sw)
{
  EXPR *cond = sw->wh_expr;
  STMT *stmt = sw->wh_stmt;
  int label_test = choose_label ();
  int label_body = choose_label ();
 
  gen_inst ("b @", label_test);
  gen_label (label_body);
  gen_stmt (stmt);
  gen_label (label_test);
  gen_expr (cond);
  do_branch (cond, 1, label_body);
}
    
void gen_stmt_repeat (STMTREPEAT *sr)
{
  EXPR *cond = sr->rpt_expr;
  STMTLIST *sl = sr->rpt_stmts;
  int label_start = choose_label () ;

  gen_label (label_start);
  gen_stmtlist (sl);
  gen_expr (cond);
  do_branch (cond, 0, label_start);
}
 
void gen_stmt_for (STMTFOR *sf)
{
  EXPR *fe = sf->for_init;
  EXPR *te = sf->for_to;
  EXPR *tmplhs = 0;
  EXPR *tmprhs = 0;
  EXPR *ielhs, *ierhs;
  int up = sf->for_upward;
  int label_start = choose_label ();
  int label_end   = choose_label ();
  int ivmin, ivmax;
  int tomin, tomax;

  /* evaluate the initial value expression */
  gen_expr (fe);

  /* synthesize an EXPR for assigning to the index variable */
  ielhs = make_expr (ExprBind_);
  { BINDING *b = sf->for_name;
    OBJECT *o;
    ielhs->e.bnd = b;
    assert (b);
    o = get_decl_object (b->bind_decl);
    assert (o);
    ielhs->expr_obj = o;
    ielhs->expr_type = o->obj_type;
    ielhs->expr_rhs = 0;
    ivmin = min_value (ielhs);
    ivmax = max_value (ielhs);
  }

  /* ... and one for using it */
  ierhs = make_expr (ExprBind_);
  { BINDING *b = sf->for_name;
    OBJECT *o;
    ierhs->e.bnd = b;
    assert (b);
    o = get_decl_object (b->bind_decl);
    assert (o);
    ierhs->expr_obj = o;
    ierhs->expr_type = o->obj_type;
    ierhs->expr_rhs = 1;
  }

  /* initialize the index variable */
  gen_assign (ielhs, fe);

  /* evaluate the TO/DOWNTO expression, if it is not a constant */
  /* also prepare expressions for referring to the calculated limit */
  /* need not fill in all fields of these "fake" expressions */
  if (get_constant_value (te) == 0) {
    OBJECT *o = sf->for_limit_obj;
    gen_expr (te);

    assert (o);
    tmprhs = make_expr (ExprBind_);
    tmprhs->expr_obj = o;
    tmprhs->expr_type = o->obj_type;
    tmprhs->expr_rhs = 1;
    
    tmplhs = make_expr (ExprBind_);
    tmplhs->expr_obj = o;
    tmplhs->expr_type = o->obj_type;
    tmplhs->expr_rhs = 0;

    gen_assign (tmplhs, te);

    tomin = min_value (te);
    tomax = max_value (te);
  }
  else {
    int val = get_const_num (te);
    tomin = val;
    tomax = val;
    tmprhs = te;
  }

  /* generate code to bypass the loop if it will have no executions */
  if (get_constant_value (fe) && get_constant_value (te)) {
    int fv = get_const_num (fe);
    int tv = get_const_num (te);
    if (( up && fv > tv) ||
	(!up && fv < tv)) { 
      warning (fe->lineno, "Warning: for loop will never execute");
      gen_inst ("b @", label_end);
    }
  }
  else {
    int ireg = get_expr_in_reg (ierhs , REG_T9);
    int treg = get_expr_in_reg (tmprhs, REG_T8);
    gen_inst ("~ % % %", (up ? "sgt" : "slt"), REG_T9, ireg, treg);
    gen_inst ("bnez % @", REG_T9, label_end);
  }
  
  /* generate the body, preceded by a label */
  gen_label (label_start);
  gen_stmt (sf->for_stmt);

  /* get, increment/decrement, and store the index variable */
  /* get the limit value, compare, and branch */
  {
    int ireg, treg;
    ireg = get_expr_in_reg (ierhs, REG_T9);
    gen_inst ("~ % % 1", (up ? "add" : "sub"), ireg, ireg);
    if (up && (tomax > ivmax)) {
      gen_inst ("sgt % % #", REG_T8, ireg, ivmax);
      gen_inst ("bnez % BADRNG", REG_T8);
    }
    else if (!up && (tomin < ivmin)) {
      gen_inst ("slt % % #", REG_T8, ireg, ivmin);
      gen_inst ("bnez % BADRNG", REG_T8);
    }
    gen_store (ireg, REG_T8, ielhs);

    treg = get_expr_in_reg (tmprhs, REG_T8);
    gen_inst ("~ % % %", (up ? "sgt" : "slt"), REG_T9, ireg, treg);
    gen_inst ("beqz % @", REG_T9, label_start);
  }

  /* label for when we're done */
  gen_label (label_end);
  return; 
}

void gen_stmt (STMT *s)
{
  if (s) {
    switch (s->tag) {
    case StmtEmpty_:
      break;
    case StmtAssign_:
      gen_stmt_assign (s->s.asn);
      break;
    case StmtCompound_:
      gen_stmtlist (s->s.comp);
      break;
    case StmtCall_:
      gen_expr_call (s->s.call); 
      break;
    case StmtIf_:
      gen_stmt_if (s->s.ifx);
      break;
    case StmtCase_:
      gen_stmt_case (s->s.cas);
      break;
    case StmtWhile_:
      gen_stmt_while (s->s.wh);
      break;
    case StmtRepeat_:
      gen_stmt_repeat (s->s.rpt);
      break;
    case StmtFor_:
      gen_stmt_for (s->s.forx);
      break;
    default:
      assert (0);
    }
  }
}

void gen_load_expr (int reg, EXPR *e)
{
  if (is_non_addr_expr (e))
    gen_load      (reg, e->expr_obj);
  else
    gen_load_addr (reg, e->expr_obj);
}

void gen_load (int reg, OBJECT *o)
{
  /* Generate code to load the datum in the location described by o */
  /* into register reg. Here are the interesting cases: */
  /*   current static level: load from offset from FP   */
  /*   outer static level: follow static chain, using register reg to */
  /*                       hold intermediate pointers, and then from  */
  /*                       offset from reg */
  /*   offset from other register: load using offset from that register */
  /* after the above, if the memory object is indirect, do an additional */
  /* load from offset 0 from reg */

  MEMORY *m;
  int level = current_plevel;
  assert (reg && o && o->tag == ObjMem_);
  m = o->o.mem;
  assert (m);
  if (m->mem_plevel == level)
    gen_inst ("lw % #(%)", reg, m->mem_offset, REG_FP);
  else if (m->mem_plevel < level) {
    gen_inst ("lw % #(%)", reg, STATIC_CHAIN, REG_FP);
    while (m->mem_plevel < --level)
      gen_inst ("lw % #(%)", reg, STATIC_CHAIN, reg);
    gen_inst ("lw % #(%)", reg, m->mem_offset, reg);
  }
  else if (m->mem_plevel >= 1000)
    gen_inst ("lw % #(%)", reg, m->mem_offset, m->mem_plevel-1000);
  else
    assert (0);
  if (m->mem_indirect)
    gen_inst ("lw % 0(%)", reg, reg);
}

void gen_load_addr (int reg, OBJECT *o)
{
  /* Get the ADDRESS of the memory area described by o into reg. */
  /* Cases as follows: */
  /*   if o is indirect, "fake out" gen_load to do the load for us */
  /*   current static level: use a load-address to add offset to FP */
  /*   outer static level: follow the static chain first, then use a */
  /*                       load-address to form the address */
  /*   offset from a specific register: use load-address of offset */
  /*                       from that register */
  MEMORY *m;
  int level = current_plevel;
  assert (reg && o && o->tag == ObjMem_);
  m = o->o.mem;
  assert (m);
  if (m->mem_indirect) {
    m->mem_indirect = 0;
    gen_load (reg, o);
    m->mem_indirect = 1;
  }
  else if (m->mem_plevel == level) {
    if (m->mem_offset)
      gen_inst ("la % #(%)", reg, m->mem_offset, REG_FP);
    else if (reg != REG_FP)
      gen_inst ("move % %", reg, REG_FP);
  }
  else if (m->mem_plevel < level) {
    gen_inst ("lw % #(%)", reg, STATIC_CHAIN, REG_FP);
    while (m->mem_plevel < --level)
      gen_inst ("lw % #(%)", reg, STATIC_CHAIN, reg);
    if (m->mem_offset)
      gen_inst ("la % #(%)", reg, m->mem_offset, reg);
  }
  else if (m->mem_plevel >= 1000) {
    if (m->mem_offset)
      gen_inst ("la % #(%)", reg, m->mem_offset, m->mem_plevel-1000);
    else if (reg != m->mem_plevel-1000)
      gen_inst ("move % %", reg, m->mem_plevel-1000);
  }
  else
    assert (0);
}

void gen_store (int reg, int scratch, EXPR *e)
{
  /* store register reg into e's memory home; scratch is a register */
  /*   available for holding intermediate values */
  /* e is an expression rather than an object so that we can see its */
  /*   register allocation information */
  /* Note that in the indirect case, we definitely need scratch, to */
  /*   hold the address being stored into; it is also useful in the */
  /*   case of outer scope references. */
  
  OBJECT *o = e->expr_obj;
  MEMORY *m;
  int level = current_plevel;
  assert (reg && o && o->tag == ObjMem_
	  && (e->expr_rhs == 0 || e->reg_spill == 1));
  m = o->o.mem;
  assert (m);
  if (e->reg_name && e->reg_spill == 0) {
    /* register contains the necessary address */
    gen_inst ("sw % 0(%)", reg, e->reg_name);
  }
  else if (level == m->mem_plevel) {
    if (m->mem_indirect) {
      gen_inst ("lw % #(%)", scratch, m->mem_offset, REG_FP);
      gen_inst ("sw % 0(%)", reg, scratch);
    }
    else
      gen_inst ("sw % #(%)", reg, m->mem_offset, REG_FP);
  }
  else if (level > m->mem_plevel) {
    gen_inst ("lw % #(%)", scratch, STATIC_CHAIN, REG_FP);
    while (--level > m->mem_plevel)
      gen_inst ("lw % #(%)", scratch, STATIC_CHAIN, scratch);
    if (m->mem_indirect) {
      gen_inst ("lw % #(%)", scratch, m->mem_offset, scratch);
      gen_inst ("sw % 0(%)", reg, scratch);
    }
    else
      gen_inst ("sw % #(%)", reg, m->mem_offset, scratch);
  }
  else if (m->mem_plevel >= 1000) {
    if (m->mem_indirect) {
      gen_inst ("lw % #(%)", scratch, m->mem_offset, m->mem_plevel-1000);
      gen_inst ("sw % 0(%)", reg, scratch);
    }
    else
      gen_inst ("sw % #(%)", reg, m->mem_offset, m->mem_plevel-1000);
  }
  else
    assert (0);
}

int get_expr_in_reg (EXPR *e, int scratch)
{
  /* This is used in expression calculation, to get operands into registers */
  /* prior to emitting instructions operating on them. The result returned */
  /* is the number of the register used, which is the one assigned to e, */
  /* unless e was spilled to memory, or the scratch register passed in. */
  OBJECT *o;
  assert (e);
  o = e->expr_obj;
  assert (o);
  if (e->reg_name && e->reg_spill == 0)
    return e->reg_name;
  switch (o->tag) {
  case ObjValue_: {
    VALUE *v = o->o.val;
    assert (v);
    switch (v->tag) {
    case ValueInt_:
      gen_inst ("li % #", scratch, v->v.intx);
      return scratch;
    case ValueStr_: {
      TEXT *tt = v->v.text;
      assert (tt);
      gen_inst ("la % LS#", scratch, tt->text_number);
      return scratch;
    }
    default:
      break;
    }
  }
  case ObjMem_: {
    gen_load (scratch, o);
    return scratch;
  }
  default:
    break;
  }
  assert (0);
  return 0; /* keeps gcc happy */
}

int get_addr_in_reg (EXPR *e, int scratch)
{
  OBJECT *o;
  assert (e);
  o = e->expr_obj;
  assert (o);
  if (e->reg_name) {
    if (e->reg_spill == 0)
      return e->reg_name;
    scratch = e->reg_name;
  }
  gen_load_addr (scratch, o);
  return scratch;
}

/* implements a table of instructions for simple binary operators */
char *get_bin_opcode (int op)
{
  switch (op) {
  case '+':  return "add";
  case '-':  return "sub";
  case '*':  return "mulo";
  case DIV_: return "div";
  case MOD_: return "rem";
  case AND_: return "and";
  case OR_:  return "or";
  case '=':  return "seq";
  case NE_:  return "sne";
  case '>':  return "sgt";
  case GE_:  return "sge";
  case '<':  return "slt";
  case LE_:  return "sle";
  default:   return 0;
  }
}

char *is_immed (int op, EXPR *e)
{
  if (get_constant_value (e)) {
    /* assembler will deal with limitations on constant size */
    switch (op) {
    case '+':  return "add";
    case '-':  return "sub";
    case '*':  return "mulo";
    case DIV_: return "div";
    case MOD_: return "rem";
    case '<':  return "slt";
    case '>':  return "sgt";
    case LE_:  return "sle";
    case GE_:  return "sge";
    case '=':  return "seq";
    case NE_:  return "sne";
    case AND_: return "and";
    case OR_:  return "or";
    default:   break;
    }
  }
  return 0;
}

char *is_left_immed (int op, EXPR *e)
{
  if (get_constant_value (e)) {
    /* can allow only commutative operators or ones with reverse sense */
    switch (op) {
    case '+':
    case '*':
    case '=':
    case NE_:
    case AND_:
    case OR_:
      /* these operators are commutative */
      return is_immed (op, e);

    /* these operators have "reverse" versions (operands in reverse order) */
    case '<':  return "sgt";
    case '>':  return "slt";
    case LE_:  return "sge";
    case GE_:  return "sle";
    default:   break;
    }
  }
  return 0;
}

void gen_expr_subscript (EXPR *e, EXPR *l, EXPR *r)
{
  OBJECT *o, *ol;
  MEMORY *m, *ml;
  int reg;

  assert (e && l);
  o = e->expr_obj;
  ol = l->expr_obj;
  assert (o && o->tag == ObjMem_ && ol && ol->tag == ObjMem_);
  m = o->o.mem;
  ml = ol->o.mem;
  assert (m && ml);
  reg = e->reg_name;
  if (get_constant_value (r)) {
    int offset = get_element_offset (l, r);
    if (offset < 0) {
      gen_expr (l);
      gen_inst ("b BADSUB");
      return;
    }
    if (m->mem_plevel >= 1000) {
      gen_expr (l);
      if (reg && e->reg_spill == 0)
	gen_load_expr (reg, e);
      return;
    }
    else if (m->mem_indirect) {
      int lreg;
      assert (reg);
      gen_expr (l);
      lreg = get_addr_in_reg (l, reg);     /* determine left's reg */
      if (reg && is_non_addr_expr (e))
	gen_inst ("lw % #(%)", reg, offset, lreg);  /* the value case */
      else if (offset)
	gen_inst ("add % % #", reg, lreg, offset);  /* address cases */
      else if (reg != lreg)
	gen_inst ("move % %", reg, lreg);
      return;
    }
    else if (reg)
      gen_load_expr (reg, e);
    return;
  }
  else {
    int lreg, rreg, lo, hi, size;
    TYPE *at = l->expr_type, *it;
    assert (at && at->tag == TypeArray_ && at->t.arr && reg);
    it = at->t.arr->arr_index;
    assert (it);
    lo = index_lo_num (it);
    hi = index_hi_num (it);

    /* evaluate array and subscript */
    if (l->expr_later) {
      gen_expr (r);
      gen_expr (l);
    }
    else {
      gen_expr (l);
      gen_expr (r);
    }

    /* perform bounds check */
    rreg = get_expr_in_reg (r, REG_T9);
    if (lo)
      gen_inst ("sub % % #", rreg, rreg, lo);
    if (min_value (r) < lo)
      gen_inst ("bltz % BADSUB", rreg);
    if (max_value (r) > hi) {
      gen_inst ("sgt % % #", REG_T8, rreg, hi - lo);
      gen_inst ("bnez % BADSUB", REG_T8);
    }
    size = e->expr_type->size;
    assert (size);
    if (size != 1) {
      if ((size & (size - 1)) == 0) {
	/* size is a power of 2: use a shift */
	int count = 0;
	while (size > 1) {
	  count += 1;
	  size >>= 1;
	}
	gen_inst ("sll % % #", rreg, rreg, count);
      }
      else
	gen_inst ("mul % % #", rreg, rreg, size);
    }
    lreg = get_addr_in_reg (l, REG_T8);
    gen_inst ("add % % %", reg, lreg, rreg);
    if (is_non_addr_expr (e))
      gen_inst ("lw % 0(%)", reg, reg);
  }
}

void gen_expr_field (EXPR *e, EXPR *l, EXPR *r)
{
  OBJECT *o, *ol;
  MEMORY *m, *ml;
  int reg;
  assert (e && l);
  o = e->expr_obj;
  ol = l->expr_obj;
  assert (o && o->tag == ObjMem_ && ol && ol->tag == ObjMem_);
  m = o->o.mem;
  ml = ol->o.mem;
  assert (m && ml);
  reg = e->reg_name;
  if (m->mem_plevel >= 1000) {
    gen_expr (l);
    if (reg && e->reg_spill == 0)
      gen_load_expr (reg, e);
    return;
  }
  if (reg) {
    int lreg;
    int offset = get_field_offset (l, r);
    gen_expr (l);
    lreg = get_addr_in_reg (l, reg);
    if (is_non_addr_expr (e))
      gen_inst ("lw % #(%)", reg, offset, lreg);
    else if (offset)
      gen_inst ("add % % #", reg, lreg, offset);
    else if (reg != lreg)
      gen_inst ("move % %", reg, lreg);
  }
  return;
}

void gen_expr_binop (int op, EXPR *l, EXPR *r, int reg)
{
  char *opcode;
  assert (reg);
  if ((opcode = is_immed (op, r))) {
    int lreg = get_expr_in_reg (l, REG_T9);
    int num = get_const_num (r);
    if (num == 0 && ((op == DIV_) || (op == MOD_)))
      gen_inst ("b BADDIV");
    gen_inst ("~ % % #", opcode, reg, lreg, num);
  }
  else if ((opcode = is_left_immed (op, l))) {
    int rreg = get_expr_in_reg (r, REG_T9);
    int num = get_const_num (l);
    if (op == DIV_ || op == MOD_)
      gen_inst ("beqz % BADDIV", rreg);
    gen_inst ("~ % % #", opcode, reg, rreg, num);
  }
  else if ((opcode = get_bin_opcode (op))) {
    int lreg = get_expr_in_reg (l, REG_T9);
    int rreg = get_expr_in_reg (r, REG_T8);
    if (op == DIV_ || op == MOD_)
      gen_inst ("beqz % BADDIV", rreg);
    gen_inst ("~ % % %", opcode, reg, lreg, rreg);
  }
  else
    assert (0);
}

void gen_expr_deref (EXPR *e, EXPR *eu)
{
  OBJECT *o;
  MEMORY *m;
  int reg, ureg;
  assert (e);
  o = e->expr_obj;
  assert (o && o->tag == ObjMem_);
  m = o->o.mem;
  assert (m);
  reg = e->reg_name;
  gen_expr (eu);
  assert (reg);
  ureg = get_expr_in_reg (eu, reg);
  gen_inst ("beqz % BADPTR", ureg);
  if (is_non_addr_expr (e))
    gen_inst ("lw % 0(%)", reg, ureg);
  else if (ureg != reg)
    gen_inst ("move % %", reg, ureg);
}

void gen_expr_unop (int op, EXPR *e, int reg)
{
  switch (op) {
  case '+': {
    int ureg = get_expr_in_reg (e, reg);
    if (ureg != reg)
      gen_inst ("move % %", reg, ureg);
    return;
  }
  case '-': {
    int ureg = get_expr_in_reg (e, reg);
    gen_inst ("neg % %", reg, ureg);
    return;
  }
  case NOT_: {
    int ureg = get_expr_in_reg (e, reg);
    gen_inst ("xori % % 1", reg, ureg);
    return;
  }
  default:
    assert (0);
  }
}

void gen_expr_special (DECL *d, EXPRLIST *actuals)
{
  SPECIALWHICH spc;
  assert (d && d->tag == DeclSpecial_);
  spc = d->d.spc;
  switch (spc) {
  case SpecialWrite_:
  case SpecialWriteln_:{
    for ( ; actuals; actuals = actuals->rest) {
      EXPR *actual = actuals->this;
      int reg;
      gen_expr (actual);
      reg = get_expr_in_reg (actual, REG_A0);
      if (reg != REG_A0)
	gen_inst ("move % %", REG_A0, reg);
      gen_inst ("jal ~", (is_txt_type (actual->expr_type) ? "wstr" : "wint"));
    }
    if (spc == SpecialWriteln_)
      gen_inst ("jal wln");
    return;
  }
  case SpecialReadln_: {
    int count;
    char *format, *fp;
    EXPRLIST *temp = actuals;
    TEXT *txt;
    for (count = 0; temp; temp = temp->rest)
      ++count;
    /* make room for arguments */
    gen_inst ("sub % % #", REG_SP, REG_SP, (count + 2) << 2);
    gen_inst ("ble % % STKOV", REG_SP, REG_SL);

    /* prepare to build format string */
    format = (char *)(malloc (count * 3 + 2));
    fp = format;

    for (count = 0; actuals; actuals = actuals->rest) {
      EXPR *actual = actuals->this;
      int reg;
      gen_expr (actual);
      reg = get_addr_in_reg (actual, REG_T9);
      gen_inst ("sw % #(%)", reg, (++count + 1) << 2, REG_SP);
      *fp++ = ' ';
      *fp++ = '%';
      *fp++ = 'd';
    }
    *fp++ = '\n';
    *fp = 0;
    txt = text_find_insert (format);
    free (format);
    gen_inst ("la % LS#", REG_T9, txt->text_number);
    gen_inst ("sw % 4(%)", REG_T9, REG_SP);
    gen_inst ("li % #", REG_T9, count);
    gen_inst ("sw % 0(%)", REG_T9, REG_SP);

    gen_inst ("li % 12", REG_V0);
    gen_inst ("move % %", REG_A0, REG_SP);
    gen_inst ("syscall");
    gen_inst ("add % % #", REG_SP, REG_SP, (count + 2) << 2);
    return;
  }
  case SpecialNew_: {
    EXPR *actual;
    TYPE *at, *it, *pt;
    BINDING *b;
    DECL *d;
    assert (actuals);
    actual = actuals->this;
    assert (actual);
    at = actual->expr_type;
    assert (at && at->tag == TypePointer_);
    it = at->t.ptr;
    assert (it && it->tag == TypeIdent_);
    b = it->t.bind;
    assert (b);
    d = b->bind_decl;
    assert (d && d->tag == DeclType_);
    pt = d->d.typ;
    assert (pt);
    gen_expr (actual);
    gen_inst ("li % #", REG_A0, pt->size);
    gen_inst ("jal alloc");
    gen_store (REG_A0, REG_T9, actual);
    return;
  }
  }
}

void gen_expr_call (EXPR *e)
{
  EXPRCALL *c;
  BINDING *name;
  DECL *d;
  PROCFUNCDEF *p;
  EXPRLIST *actuals;
  DECLLIST *formals;
  int size;
  EXPR *target; /* for setting up saving of each argument */

  assert (e);
  c = e->e.call;
  assert (c);
  name = c->call_name;
  assert (name);
  d = name->bind_decl;
  assert (d);
  if (d->tag == DeclSpecial_) {
    gen_expr_special (d, c->call_actuals);
    return;
  }
  assert (d->tag == DeclProcFunc_);
  p = d->d.proc;
  assert (p);
  size = p->proc_formals_result_size;
  assert (size);
  formals = p->proc_formals;

  /* first, allocate space for frame */
  gen_inst ("sub % % #", REG_SP, REG_SP, size);
  gen_inst ("ble % % STKOV", REG_SP, REG_SL);

  /* set up target for argument processing */
  target = make_expr (ExprBind_);
  target->expr_rhs = 0;

  /* evaluate and save actuals */
  for (actuals = c->call_actuals; actuals; actuals = actuals->rest,
                                           formals = formals->rest) {
    EXPR *actual = actuals->this;
    DECL *formal = formals->this;
    FORMAL *f;
    OBJECT *o;
    MEMORY *m;
    assert (formal && formal->tag == DeclFormal_);
    f = formal->d.frm;
    assert (f);
    o = f->frm_addr;
    assert (o && o->tag == ObjMem_);
    m = o->o.mem;
    assert (m);
    gen_expr (actual);
    if (f->frm_mode == ModeVar_) {
      int reg = get_addr_in_reg (actual, REG_T9);
      gen_inst ("sw % #(%)", reg, m->mem_offset, REG_SP);
    }
    else {
      target->expr_obj =
	make_mem_object (f->frm_type, 0, 0, 1000+REG_SP, m->mem_offset);
      target->expr_type = f->frm_type;
      gen_assign (target, actual);
    }
  }

  /* compute and save new static chain */
  {
    int level = p->proc_syminfo->symtab_plevel;
    if (level > current_plevel)
      gen_inst ("sw % #(%)", REG_FP, STATIC_CHAIN, REG_SP);
    else {
      gen_inst ("lw % #(%)", REG_T9, STATIC_CHAIN, REG_FP);
      while (++level <= current_plevel)
	gen_inst ("lw % #(%)", REG_T9, STATIC_CHAIN, REG_T9);
      gen_inst ("sw % #(%)", REG_T9, STATIC_CHAIN, REG_SP);
    }
  }

  /* do the call */
  gen_inst ("jal @", p->proc_top_label);

  /* obtain results, if any */
  if (e->expr_type) {
    OBJECT *fo = p->proc_result_addr, *ro;
    MEMORY *fm;
    assert (fo && fo->tag == ObjMem_);
    fm = fo->o.mem;
    assert (fm);
    ro = make_mem_object (e->expr_type, 0, 0, 1000+REG_SP, fm->mem_offset);
    if (e->reg_name)
      gen_load (e->reg_name, ro);
    else {
      EXPR *er = make_expr (ExprBind_);
      er->expr_obj = ro;
      er->expr_type = e->expr_type;
      er->expr_rhs = 1;
      gen_assign (e, er);
    }
  }

  /* deallocate stack space for arguments */
  gen_inst ("add % % #", REG_SP, REG_SP, size);
}

void gen_expr (EXPR *e)
{
  if (e == 0 || e->expr_type == the_err_type)
    return;
  if (e->expr_obj && e->expr_obj->tag == ObjValue_)
    return;
  switch (e->tag) {
  case ExprBinary_: {
    EXPRBINARY *b = e->e.bin;
    EXPR *l, *r;
    int op;
    assert (b);
    l = b->bin_left;
    r = b->bin_right;
    op = b->bin_op;
    assert (l && r && op);
    switch (op) {
    case '[': gen_expr_subscript (e, l, r); break;
    case '.': gen_expr_field     (e, l, r); break;
    default:
      if (r->expr_later) {
	gen_expr (l);
	gen_expr (r);
      }
      else {
	gen_expr (r);
	gen_expr (l);
      }
      gen_expr_binop (b->bin_op, l, r, e->reg_name);
      break;
    }
    break;
  } 
  case ExprUnary_: {
    EXPRUNARY *u = e->e.un;
    EXPR *eu;
    int op;
    assert (u);
    eu = u->un_expr;
    op = u->un_op;
    assert (eu && op);
    if (op == '^')
      gen_expr_deref (e, eu);
    else {
      gen_expr (eu);
      gen_expr_unop (op, eu, e->reg_name);
    }
    break;
  }
  case ExprBind_:
    if (e->reg_name)
      gen_load_expr (e->reg_name, e);
    break;
  case ExprInt_:
  case ExprText_:
  case ExprNil_:
  case ExprIdent_:
    /* all of these will get loaded as needed */
    break;
  case ExprCall_: 
    gen_expr_call (e);
    break;
  default:
    break;
  }
  /* handle spill here */
  if (e->reg_name && e->reg_spill == 1)
    gen_store (e->reg_name, REG_T9, e);
  return;
}
