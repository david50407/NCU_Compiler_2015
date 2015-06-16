/* Table driven scanner */
# include "project.h"
# include "parse.tab.h"

enum Class {
  EOF_ = 0,  /* special end-of-file token (0 required by bison) */
  SPC_,      /* white space character */
  NL_,       /* newline/carriage return */
  LET_,      /* letters */
  DIG_,      /* digits */
  OP_,       /* single character operators */
  QUO_,      /* ' */
  AST_,      /* Asterisk: * */
  LP_,       /* Left Parenthesis: ( */
  RP_,       /* Right Parenthesis: ) */
  DOT_,      /* . */
  LC_,       /* Left Curly bracket: { */
  RC_,       /* Right Curly bracket: } */
  EQ_,       /* = */
  LT_,       /* < */
  GT_,       /* > */
  COL_,      /* : */
  OTH_,      /* other legal characters */
  BAD_       /* illegal characters (must be last for NCLASS) */
};

# define NCLASS ((int)(BAD_)+1)  /* number of char classes */

/* This table has one entry for each possible byte value, 0 through 255,
   preceded by an entry for end of file at what is the -1 entry for the
   pointer used as a table base. */
static struct {
  enum Class eof;
  enum Class classes[256];
} class_table = {
  EOF_,
  {
    /* white space */
    [' '] = SPC_,
    ['\t'] = SPC_,
    /* newline/carriage return */
    ['\n'] = NL_,
    ['\r'] = NL_,
    /* letters */
    ['A'] = LET_, ['B'] = LET_, ['C'] = LET_, ['D'] = LET_, ['E'] = LET_,
    ['F'] = LET_, ['G'] = LET_, ['H'] = LET_, ['I'] = LET_, ['J'] = LET_,
    ['K'] = LET_, ['L'] = LET_, ['M'] = LET_, ['N'] = LET_, ['O'] = LET_,
    ['P'] = LET_, ['Q'] = LET_, ['R'] = LET_, ['S'] = LET_, ['T'] = LET_,
    ['U'] = LET_, ['V'] = LET_, ['W'] = LET_, ['X'] = LET_, ['Y'] = LET_,
    ['Z'] = LET_,
    ['a'] = LET_, ['b'] = LET_, ['c'] = LET_, ['d'] = LET_, ['e'] = LET_,
    ['f'] = LET_, ['g'] = LET_, ['h'] = LET_, ['i'] = LET_, ['j'] = LET_,
    ['k'] = LET_, ['l'] = LET_, ['m'] = LET_, ['n'] = LET_, ['o'] = LET_,
    ['p'] = LET_, ['q'] = LET_, ['r'] = LET_, ['s'] = LET_, ['t'] = LET_,
    ['u'] = LET_, ['v'] = LET_, ['w'] = LET_, ['x'] = LET_, ['y'] = LET_,
    ['z'] = LET_,
    /* digits */
    ['0'] = DIG_, ['1'] = DIG_, ['2'] = DIG_, ['3'] = DIG_, ['4'] = DIG_,
    ['5'] = DIG_, ['6'] = DIG_, ['7'] = DIG_, ['8'] = DIG_, ['9'] = DIG_,
    /* single-character operators */
    [';'] = OP_,
    [','] = OP_,
    ['^'] = OP_,
    ['-'] = OP_,
    ['+'] = OP_,
    ['['] = OP_,
    [']'] = OP_,
    /* special characters */
    ['*'] = AST_,
    ['('] = LP_,
    [')'] = RP_,
    ['\''] = QUO_,
    ['.'] = DOT_,
    ['='] = EQ_,
    ['{'] = LC_,
    ['}'] = RC_,
    [':'] = COL_,
    ['<'] = LT_,
    ['>'] = GT_,
    /* other legal characters */
    ['!'] = OTH_, ['@'] = OTH_,
    ['#'] = OTH_, ['$'] = OTH_,
    ['%'] = OTH_, ['&'] = OTH_,
    ['_'] = OTH_, ['\\'] = OTH_,
    ['|'] = OTH_, ['`'] = OTH_,
    ['~'] = OTH_, ['"'] = OTH_,
    ['/'] = OTH_, ['?'] = OTH_,
  }
};

static enum Class *classes = &(class_table.classes[0]);

enum State {
  SBad_ = -1,  /* not a real state: indicates no transition */
  SBeg_ =  0,  /* starting state (discarding) */
  SId_ ,       /* in an identifier (accepting) */
  SOp_ ,       /* an operator (accepting) */
  SLt_ ,       /* saw a < */
  SGC_ ,       /* saw > or : */
  SCom_,       /* in a comment (discarding) */
  SNum_,       /* in a number (accepting) */
  SDot_,       /* saw a . */
  SQuo_,       /* string start: ' */
  SStr_,       /* in a string (at least one character) */
  SSQ_ ,       /* in a string, saw a quote */
  SQQ_ ,       /* string start plus an additional quote */
  SLP_ ,       /* saw a left parenthesis */
  SCA_ ,       /* saw an asterisk in a comment */
  SEOF_        /* saw end of file; must be last for NSTATE */
};
# define NSTATE ((int)(SEOF_)+1)  /* number of (non-Bad) states */


/* data structures for transition table */

static enum State next_state[NSTATE][NCLASS];

/*
   the actual transition table is built during initialization, from this list
   of the "interesting" (non-Bad) transitions:
*/

typedef struct transition {
  enum State  from;
  enum Class  input;
  enum State  to;
} TRANSITION;

static TRANSITION transitions[] = {
  {SBeg_, EOF_, SEOF_},  /* start: end of file */
  {SBeg_, SPC_, SBeg_},  /* start: white space */
  {SBeg_, NL_ , SBeg_},  /* start: newline/CR */
  {SBeg_, LET_, SId_ },  /* start: letter => get identifier */
  {SBeg_, DIG_, SNum_},  /* start: digit => get number */
  {SBeg_, OP_ , SOp_ },  /* start: single char operator */
  {SBeg_, EQ_ , SOp_ },  /* start: single char operator */
  {SBeg_, RP_ , SOp_ },  /* start: single char operator */
  {SBeg_, AST_, SOp_ },  /* start: single char operator */
  {SBeg_, LT_ , SLt_ },  /* start: single/double char operator */
  {SBeg_, GT_ , SGC_ },  /* start: single/double char operator */
  {SBeg_, COL_, SGC_ },  /* start: single/double char operator */
  {SBeg_, DOT_, SDot_},  /* start: single/double char operator */
  {SBeg_, QUO_, SQuo_},  /* start: quote => string */
  {SBeg_, LC_ , SCom_},  /* start: left curly bracket => comment */
  {SBeg_, LP_ , SLP_ },  /* start: left paren => may start comment */

  {SId_ , LET_, SId_ },  /* identifiers continue with LET or DIG */
  {SId_ , DIG_, SId_ },

  {SNum_, DIG_, SNum_},  /* numbers continue with DIG_ */

  /* SOp_ has no outgoing transitions */

  {SLt_ , EQ_ , SOp_ },  /* double char operator <= */
  {SLt_ , GT_ , SOp_ },  /* double char operator <> */

  {SGC_ , EQ_ , SOp_ },  /* double char operators >= and := */

  {SDot_, DOT_, SOp_ },  /* double char operator .. */

  {SQuo_, QUO_, SQQ_ },  /* quote right after quote: "special case" */
  {SQuo_, NL_ , SBad_},  /* newline/CR not allowed in strings */
                         /* most transitions go to SStr_ (see defaults) */

  {SStr_, QUO_, SSQ_ },  /* in a string, see a quote */
  {SStr_, NL_ , SBad_},  /* newline/CR not allowed in strings */
                         /* most transitions go to SStr_ (see defaults) */

  {SSQ_ , QUO_, SStr_},  /* a quoted quote; otherwise, the string is ended */

  {SQQ_ , QUO_, SStr_},  /* a quoted quote at start of string */
                         /* note: differs from above since is not accepting */

  {SCom_, RC_ , SBeg_},  /* finish a comment */
                         /* most things continue a comment (see defaults) */
  {SCom_, AST_, SCA_ },  /* asterisk => may finish comment */

  {SLP_ , AST_, SCom_},  /* start a comment */

  {SCA_ , AST_, SCA_ },  /* another asterisk => may end comment next time */
  {SCA_ , RC_ , SBeg_},  /* end comment */
  {SCA_ , RP_ , SBeg_}   /* end comment */
                         /* most things continue a comment (see defaults) */

  /* SEOF_ has no transitions */
};

# define ntransitions ((sizeof transitions) / (sizeof (TRANSITION)))

/* for convenience, we provide the notion of a default new state for each
   state */

typedef struct deftrans {
  enum State from;
  enum State to;
} DEFTRANS;

static DEFTRANS default_transitions[] = {
  { SQuo_, SStr_ },
  { SStr_, SStr_ },
  { SCom_, SCom_ },
  { SCA_ , SCom_ }
};

# define ndeftrans ((sizeof default_transitions) / sizeof (DEFTRANS))

/* a state has a non-null action routine pointer iff it is accepting */

static int (*(state_action[NSTATE]))(void);

/* the table is initialized from these descriptions: */

typedef struct Action {
  enum State accepter;
  int (*action)(void);
} ACTION;

int process_ident   (void);
int process_number  (void);
int process_op      (void);
int return_paren    (void);
int process_string  (void);
int process_eof     (void);

static ACTION actions[] = {
  {SId_ , &process_ident  },
  {SOp_ , &process_op     },
  {SLt_ , &process_op     },
  {SGC_ , &process_op     },
  {SNum_, &process_number },
  {SDot_, &process_op     },
  {SLP_ , &return_paren   },
  {SSQ_ , &process_string },
  {SEOF_, &process_eof    }
};
# define nactions ((sizeof actions) / (sizeof (ACTION)))

/* this array indicates whether to keep input when entering each state */
static char keep_input[NSTATE];

/* list of states that keep input, for initializing above */
static enum State keepers[] = {SId_, SOp_, SLt_, SGC_, SNum_, SDot_, SStr_};

# define nkeepers ((sizeof keepers) / (sizeof (enum State)))

/* Keyword table and lookup/extract routine

   Note that the table must be in alphabetical order since it is accessed
   using binary search.  */

typedef struct keyword {
  char *symbol;
  int   code;
} KEYWORD;

static KEYWORD keytab [] = {
  {"AND"      , AND_       },
  {"ARRAY"    , ARRAY_     },
  {"BEGIN"    , BEGIN_     },
  {"CASE"     , CASE_      },
  {"CONST"    , CONST_     },
  {"DIV"      , DIV_       },
  {"DO"       , DO_        },
  {"DOWNTO"   , DOWNTO_    },
  {"ELSE"     , ELSE_      },
  {"END"      , END_       },
  {"FOR"      , FOR_       },
  {"FUNCTION" , FUNCTION_  },
  {"IF"       , IF_        },
  {"MOD"      , MOD_       },
  {"NIL"      , NIL_       },
  {"NOT"      , NOT_       },
  {"OF"       , OF_        },
  {"OR"       , OR_        },
  {"PROCEDURE", PROCEDURE_ },
  {"PROGRAM"  , PROGRAM_   },
  {"RECORD"   , RECORD_    },
  {"REPEAT"   , REPEAT_    },
  {"THEN"     , THEN_      },
  {"TO"       , TO_        },
  {"TYPE"     , TYPE_      },
  {"UNTIL"    , UNTIL_     },
  {"VAR"      , VAR_       },
  {"WHILE"    , WHILE_     }
};

static int nkeys = (sizeof keytab) / (sizeof (KEYWORD));

int
keycode (char *s)
/* Does binary search of keyword table and returns -1 if not found, otherwise
   the token number. */
{
  int lo = 0;
  int hi = nkeys - 1;
  int mid, cmp;

  while (lo <= hi) {
    mid = (lo + hi) >> 1;
    cmp = strcasecmp (s, keytab[mid].symbol);
    if (cmp == 0)
      return keytab[mid].code;
    else if (cmp < 0)
      hi = mid - 1;
    else
      lo = mid + 1;
  }
  return -1;
}

/* Identifier table hash roots; must be initialized! */
# define IDTAB_SIZE 127
static IDENT *(idtab[IDTAB_SIZE]);

IDENT *
id_find_insert (char *name)
/* identifier lookup */
{
  IDENT **p;
  char *q;
  int h;

  /* calculate hash code */
  for (h = 0, q = name; *q; h += toupper(*q++));

  /* probe and find matching entry, or leave p pointing to slot
     at which to chain a new IDENT block */
  for (p = &idtab[h % IDTAB_SIZE]; *p; p = &((*p)->id_next))
    if (strcasecmp (name, (*p)->id_chars) == 0)
      return *p;

  /* allocate and fill in new IDENT block */
  *p = (IDENT *) malloc (sizeof (IDENT));
  (*p)->id_next = 0;
  (*p)->id_curr_decl = 0;
  (*p)->id_chars = (char *) malloc (strlen (name) + 1);
  strcpy ((*p)->id_chars, name);
  return *p;
}

/* Text literal table; analogous to identifier table */
# define TXTTAB_SIZE 127
static TEXT *(txttab[TXTTAB_SIZE]);
static int text_count = 0;

TEXT *
text_find_insert (char *buf)
/* text literal lookup */
{
  TEXT **p;
  char *q;
  int h;

  /* calculate hash code */
  for (h = 0, q = buf; *q; h += *q++);

  /* probe and find matching entry, or leave p pointing to slot
     at which to chain a new TEXT block */
  for (p = &txttab[h % TXTTAB_SIZE]; *p; p = &((*p)->text_next))
    if (strcmp (buf, (*p)->text_chars) == 0)
      return *p;

  /* allocate and fill in new TEXT block */
  *p = (TEXT *) malloc (sizeof (TEXT));
  (*p)->text_next = 0;
  (*p)->text_chars = (char *) malloc (strlen (buf) + 1);
  strcpy ((*p)->text_chars, buf);
  (*p)->text_number = ++text_count;

  return *p;
}

void
text_iterate (void proc (TEXT *))
{
  int i;
  TEXT *p;
  for (i = 0; i < TXTTAB_SIZE; ++i)
    for (p = txttab[i]; p; p = p->text_next)
      proc (p);
}

static char  *tokbuf;		/* pointer to a buffer for token characters */
static int    tokbufsize;	/* its size, which may be increased if the
				   buffer overflows */
static int    tokbufcnt = 0;	/* number of characters currently in the
				   buffer */

# define INITBUFSIZE 100

/*
   'current' holds the result of the most recent fgetc call; it is an int
   so that we can represent EOF, too.

   The approach is as follows: current always holds the character *about to
   be processed* (the lookahead); it may be EOF or a real character; advance
   moves to the next character and, for convenience, returns it, too.

   yylineno (a global) keeps track of the line number for error messages
   We use eol for errors detected when end-of-line is reached, so the
   error message will not print the following line number.
*/

static int current;
static int eol = 0;

int yylineno = 1;

int
advance (void)
{
  current = fgetc (infile);
  if (eol) {
    ++yylineno;
    eol = 0;
  }
  if (current == '\n')
    eol = 1;
  return current;
}

void
append (char c)
/* Adds a character to the token buffer, growing the buffer with realloc if
   the buffer is full (we never shrink the buffer) */
{
  if (tokbufcnt == tokbufsize)
    tokbuf = (char *) realloc (tokbuf, tokbufsize += INITBUFSIZE);
  tokbuf[tokbufcnt++] = c;
}

void
scaninit (void)
/* The (only) initialization routine for the scanner. It:
   - fills in the bad slots in the class table
   - sets up the transition table, accepting action table, keep-input table
   - clears the hash table chains (identifier and text literal tables)
   - creates an initial token buffer
   - reads in the first character of the input */
{
  int i, j;
  DEFTRANS *d;
  TRANSITION *t;
  ACTION *a;
  enum State *s;

  /* set up the undefined slots in the class table */
  for (i = 256; --i >= 0; )
    if (classes[i] == 0)
      classes[i] = BAD_;

  /* set all transition table entries to SBad_ */
  for (i = NSTATE; --i >= 0; )
    for (j = NCLASS; --j >= 0; )
      next_state[i][j] = SBad_;

  /* enter the state-specific defaults, for all classes except BAD_ */
  for (d = default_transitions, i = ndeftrans; --i >= 0; ++d)
    for (j = NCLASS; --j >= 0; )
      if ((j != BAD_) && (j != EOF_))
	next_state[d->from][j] = d->to;

  /* enter the interesting transitions */
  for (t = transitions, i = ntransitions; --i >= 0; ++t)
    next_state[t->from][t->input] = t->to;

  /* set action/accept table entries to null */
  for (i = NSTATE; --i >= 0; )
    state_action[i] = 0;

  /* enter non-null action pointers */
  for (a = actions, i = nactions; --i >= 0; ++a)
    state_action[a->accepter] = a->action;

  /* clear keep_input table */
  for (i = NSTATE; --i >= 0; )
    keep_input[i] = 0;

  /* set true entries of keep_input */
  for (s = keepers, i = nkeepers; --i >= 0; )
    keep_input[*s++] = 1;

  /* clear the identifier and text tables */
  for (i =  IDTAB_SIZE; --i >= 0; )
    idtab[i] = 0;
  for (i = TXTTAB_SIZE; --i >= 0; )
    txttab[i] = 0;

  /* initialize the token buffer and current input character */
  tokbuf = (char *) malloc (INITBUFSIZE);
  advance ();
}

int
process_ident (void)
/* Routine to process identifier. It
   - returns the token code ID_ or the token code for the keyword
   - sets yylval to point to the IDENT struct if the token is an identifier */
{
  int keyc;

  append (0);  /* must null terminate string for strcasecmp to work */
  if ((keyc = keycode (tokbuf)) >= 0) {
    /* put keycode into ival for convenience */
    yylval.ival = keyc;
    return keyc;
  }
  yylval.id = id_find_insert (tokbuf);
  return ID_;
}

int
process_number (void)
{
  char *p     = tokbuf;
  char *limit = tokbuf + tokbufcnt;
  int   i = 0;

  if (classes[current] == LET_)
    errmsg (yylineno, "Expected separator between number and identifier:\n"
	    "\tunexpected character |%c| (code = %d)", current, current);
	    
  do {
    char c = *p++;
    assert (c >= '0' && c <= '9');
    i = (i * 10) + (c - '0');
  } while (p < limit);
  yylval.ival = i;
  return INT_;
}

int
process_string (void)
{
  append (0);  /* must null terminate for strcmp to work */
  yylval.tval = text_find_insert (tokbuf);
  return TEXT_;
}

int
process_op (void)
{
  /* it is helpful if operators store their token code as their value */
  if (tokbufcnt == 1)
    yylval.ival = tokbuf[0];
  else {
    int i = tokbuf[0] + (tokbuf[1] << 8);
    assert (tokbufcnt == 2);
    switch (i) {
    case '<' + ('>' << 8): yylval.ival = NE_      ; break;
    case '<' + ('=' << 8): yylval.ival = LE_      ; break;
    case '>' + ('=' << 8): yylval.ival = GE_      ; break;
    case ':' + ('=' << 8): yylval.ival = COLON_EQ_; break;
    case '.' + ('.' << 8): yylval.ival = DOT_DOT_ ; break;
    default: assert (0);
    }
  }
  return yylval.ival;
}

int
return_paren (void)
{
  return '(';
}

int
process_eof (void)
{
  return EOF_;
}

void
write_token (int token)
{
  switch (token) {
  case INT_:
    fprintf (tokfile, "#%d\n", yylval.ival); break;
  case TEXT_:
    fprintf (tokfile, "\'%s\'\n", yylval.tval->text_chars); break;
  case ID_:
    fprintf (tokfile, "$%s\n", yylval.id->id_chars); break;
  case AND_:
    fprintf (tokfile, "AND\n"); break;
  case DIV_:
    fprintf (tokfile, "DIV\n"); break;
  case DOWNTO_:
    fprintf (tokfile, "DOWNTO\n"); break;
  case GE_:
    fprintf (tokfile, ">=\n"); break;
  case LE_:
    fprintf (tokfile, "<=\n"); break;
  case MOD_:
    fprintf (tokfile, "MOD\n"); break;
  case NE_:
    fprintf (tokfile, "<>\n"); break;
  case NOT_:
    fprintf (tokfile, "NOT\n"); break;
  case OR_:
    fprintf (tokfile, "OR\n"); break;
  case TO_:
    fprintf (tokfile, "TO\n"); break;
  case ARRAY_:
    fprintf (tokfile, "ARRAY\n"); break;
  case BEGIN_:
    fprintf (tokfile, "BEGIN\n"); break;
  case CASE_:
    fprintf (tokfile, "CASE\n"); break;
  case CONST_:
    fprintf (tokfile, "CONST\n"); break;
  case DO_:
    fprintf (tokfile, "DO\n"); break;
  case ELSE_:
    fprintf (tokfile, "ELSE\n"); break;
  case END_:
    fprintf (tokfile, "END\n"); break;
  case FOR_:
    fprintf (tokfile, "FOR\n"); break;
  case FUNCTION_:
    fprintf (tokfile, "FUNCTION\n"); break;
  case IF_:
    fprintf (tokfile, "IF\n"); break;
  case NIL_:
    fprintf (tokfile, "NIL\n"); break;
  case OF_:
    fprintf (tokfile, "OF\n"); break;
  case PROCEDURE_:
    fprintf (tokfile, "PROCEDURE\n"); break;
  case PROGRAM_:
    fprintf (tokfile, "PROGRAM\n"); break;
  case RECORD_:
    fprintf (tokfile, "RECORD\n"); break;
  case REPEAT_:
    fprintf (tokfile, "REPEAT\n"); break;
  case THEN_:
    fprintf (tokfile, "THEN\n"); break;
  case TYPE_:
    fprintf (tokfile, "TYPE\n"); break;
  case UNTIL_:
    fprintf (tokfile, "UNTIL\n"); break;
  case VAR_:
    fprintf (tokfile, "VAR\n"); break;
  case WHILE_:
    fprintf (tokfile, "WHILE\n"); break;
  case COLON_EQ_:
    fprintf (tokfile, ":=\n"); break;
  case DOT_DOT_:
    fprintf (tokfile, "..\n"); break;
  case EOF_:
    fprintf (tokfile, "EOF\n"); break;
  default:
    fprintf (tokfile, "%c\n", token); break;
  }
}

int
yylex (void)
{
  enum State state = SBeg_;

  tokbufcnt = 0 ;	/* makes the token buffer empty */

  for (;;) {
    enum Class class = classes[current];
    if (class == BAD_)
      errmsg (yylineno, "Illegal character |%c| (code = %d)",
	      current, current);
    else {
      /* have a good character; what about the transition? */
      enum State next = next_state[state][class];
      if (next == SBad_) {
	/* no transition: check for acceptance */
	int (*actp)(void) = state_action[state];
	if (actp) {
	  int result = (*actp)();
	  if (tokfile)
	    write_token (result);
	  return result;
	}
	errmsg (yylineno, "Unexpected character |%c| (code = %d)",
		current, current);
	tokbufcnt = 0;  /* reset buffer */
	state = SBeg_;  /* restart scanning */
      }
      else {
	/* have a transition: make it, and append input if we should */
	state = next;
	if (keep_input[state])
	  append (current);
      }
    }
    advance ();  /* always make progress */
  }
}
