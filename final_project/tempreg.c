# include "project.h"

/* --- routines for the management of (mostly memory) temporaries --- */

/* linked list of free temporaries */
typedef struct templist {
  struct templist *next;
  int              size;
  int              level;
  int              offset;
} TEMPLIST;

static TEMPLIST *free_temps = 0;

int temp_size (TYPE *t, int indirect)
/* Return the size of an object of type t. Indirect objects always take
   4 bytes, the size of a pointer. */
{
  assert (t);
  return (indirect ? 4 : t->size);
}

OBJECT *alloc_temp (TYPE *t, int indirect)
/* Allocates a temporary of type t */
{
  TEMPLIST *prev, *curr;
  int size = temp_size (t, indirect);
  int offset;

  /* Check free list for available temporary of appropriate size and level */
  for (prev = 0, curr = free_temps; curr; prev = curr, curr = curr->next)
    if (curr->size  == size &&
	curr->level == current_symtab->symtab_level)
      break;

  /* If free temporary available, re-use it ... */
  if (curr) {
    offset = curr->offset;
    /* and delete from free list */
    if (prev)
      prev->next = curr->next;
    else
      free_temps = curr->next;
    free (curr);
  }
  /* Otherwise allocate new temporary variable in current activation record */
  else
    offset = alloc_variable (size);
  return make_mem_object (t, 1, /* default: 1 to set, one to use */
			  indirect, current_symtab->symtab_plevel, offset);
}

void free_temp (OBJECT *o)
/* Free temporary variable if no longer needed, i.e. mem_temp equal to 1.
   If mem_temp is greater than 0 and less than 100, decrement it. */
{
  MEMORY *m;
  int tmp;
  if (o == 0 || o->tag != ObjMem_)
    return;
  m = o->o.mem;
  assert (m);
  tmp = m->mem_temp;
  if (tmp == 0 || tmp == 100)
    return;
  else if (--m->mem_temp == 0) {
    /* Add temporary to free list */
    TEMPLIST *new = anew (TEMPLIST);
    new->level  = current_symtab->symtab_level;
    new->offset = m->mem_offset;
    new->size   = temp_size (o->obj_type, m->mem_indirect);
    new->next  = free_temps;
    free_temps = new;
  }
}

void destroy_temps ()
/* Eliminate any temporaries for the current procedure level from free_temps
   list. Must be called when exiting a procedure scope so that temporary
   allocation in the next procedure at that level will start fresh. */
{
  TEMPLIST *old = free_temps;
  int level = current_symtab->symtab_plevel;

  free_temps = 0;
  while (old) {
    TEMPLIST *next = old->next;
    if (old->level < level) {
      old->next = free_temps;
      free_temps = old;
    }
    else
      free (old);
    old = next;
  }
}

/**********************************************************************/
/*                  Register declarations                             */
/**********************************************************************/

typedef struct  RegInfo {
  int reg_avail; /* is the register available for use?  */
  int next;      /* which register is next in list */
} REGINFO; 

char *reg_pname[TOTAL_REGS] = {
  /* register print names */
  "$0"  , "$at" , "$v0" , "$v1" ,
  "$a0" , "$a1" , "$a2" , "$a3" ,
  "$t0" , "$t1" , "$t2" , "$t3" ,
  "$t4" , "$t5" , "$t6" , "$t7" ,
  "$s0" , "$s1" , "$s2" , "$s3" ,
  "$s4" , "$s5" , "$s6" , "$s7" ,
  "$t8" , "$t9" , "$kt0", "$kt1",
  "$gp" , "$sp" , "$fp" , "$ra"
};

static REGINFO RegTable[TOTAL_REGS];
static reg_first_free = -1;

static int allocable_regs[] = {
  REG_T0, REG_T1, REG_T2, REG_T3,
/* may allow use of more registers later ... */
/*
  REG_T4, REG_T5, REG_T6, REG_T7,
  REG_T8, REG_T9,

  REG_S0, REG_S1, REG_S2, REG_S3,
  REG_S4, REG_S5, REG_S6, REG_S7,

  REG_A0, REG_A1, REG_A2, REG_A3,
  REG_V0, REG_V1,

  REG_GP,
*/
};
  
int regs_available = 0;

void init_regtable ()
{
  REGINFO *p = &RegTable[0];
  int i;
  for (i = 0; i < TOTAL_REGS; ++i, ++p) {
    p->reg_avail = 0;
    p->next = -1;
  }
  for (i = (sizeof allocable_regs / sizeof (int)); --i >= 0; )
    reg_add_to_free_list (allocable_regs[i]);
}

void reg_add_to_free_list (int r)
{
  REGINFO *p = &RegTable[r];
  p->next = reg_first_free;
  reg_first_free = r;
  p->reg_avail = 1;
  ++regs_available;
}

void alloc_reg (EXPR *e)
{
  REGINFO *p;

  assert (reg_first_free >= 0); /* should not run out of registers, by design! */
  assert (e);
  e->reg_name = reg_first_free;
  p = &RegTable[reg_first_free];
  reg_first_free = p->next;
  p->next = -1;
  assert (p->reg_avail);
  p->reg_avail = 0;
  --regs_available;
}

void free_reg (EXPR *e)
{
  REGINFO *p;
  assert (e);
  if (e->reg_name == 0 || e->reg_spill == 1)
    return;
  p = &RegTable[e->reg_name];
  assert (p->reg_avail == 0 && p->next == -1);
  reg_add_to_free_list (e->reg_name);
}

static int label_counter = 0;

int choose_label ()
{
  return ++label_counter;
}
