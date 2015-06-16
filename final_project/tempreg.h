# define TOTAL_REGS 32
# define ALLOC_REGS 4
/* above should correspond with number of registers given to allocator */

/* definitions of register names/numbers */
/* comments give the MIPS standard usage */

# define REG_0    0 /* always produces the value 0 when read */
# define REG_AT   1 /* Assembler Temporary (other use prohibited) */
# define REG_V0   2 /* Value 0-1: for function return values */
# define REG_V1   3
# define REG_A0   4 /* Argument 0-3: for subroutine arguments */
# define REG_A1   5
# define REG_A2   6
# define REG_A3   7
# define REG_T0   8 /* Temporary 0-9: for temporaries not saved over calls */
# define REG_T1   9
# define REG_T2  10
# define REG_T3  11
# define REG_T4  12
# define REG_T5  13
# define REG_T6  14
# define REG_T7  15
# define REG_T8  24
# define REG_T9  25
# define REG_S0  16 /* Saved 0-8: saved over calls */
# define REG_S1  17
# define REG_S2  18
# define REG_S3  19
# define REG_S4  20
# define REG_S5  21
# define REG_S6  22
# define REG_S7  23
# define REG_S8  30
# define REG_KT0 26 /* Kernel Temporary 0-1: kernel use only */
# define REG_KT1 27
# define REG_GP  28 /* Global Pointer: address of global area */
# define REG_SP  29 /* Stack Pointer */
# define REG_RA  31 /* Return Address: set by call instructions */

/* overview of OUR usage of the registers: */
/* SP: stack pointer */
/* RA: return address */
/* S8: frame pointer */
/* S7: stack limit */
/* T0-T3: expression temporaries */
/* T6-T9: for code generator use */
/* rest: unused, or possibly expression temporaries */

# define REG_FP  REG_S8
# define REG_SL  REG_S7

/* some additional useful definitions */

# define MAX_FRAME_SIZE 32767
# define LINKAGE_SIZE 12

/* interfaces of routines */

extern int temp_size (TYPE *t, int indirect);
extern OBJECT *alloc_temp (TYPE *t, int indirect);
extern void free_temp (OBJECT *o);
extern void destroy_temps ();

extern char *reg_pname[TOTAL_REGS];
extern int regs_available; /* number of registers currently available */

extern void init_regtable ();
extern void reg_add_to_free_list (int r);
extern void alloc_reg (EXPR *e);
extern void free_reg (EXPR *e);

extern int choose_label ();
