# include <stdlib.h>
# include <stdio.h>
extern int      fgetc(FILE *);
extern int      fprintf(FILE *, const char *, ...);
//extern int      vfprintf(FILE *, const char *, void *);
extern int      fclose(FILE *);
extern int      printf(const char *, ...);
extern int	_flsbuf();
# include <stdarg.h>
# include <assert.h>
# include <string.h>
extern int strcasecmp(const char *, const char *);
# include <ctype.h>
extern int toupper(int);
# include "structs.h"
# include "mpc.h"
# include "scan.h"
# include "parse.h"
# include "astree.h"
# include "types.h"
# include "exprs.h"
# include "objects.h"
# include "values.h"
# include "symtab.h"
# include "decls.h"
# include "stmts.h"
# include "tempreg.h"
# include "gen_lib.h"
# include "gen_code.h"
