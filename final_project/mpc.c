# include "project.h"

char *inname;			/* source file name for error messages */
FILE *infile;			/* source file */
FILE *asmout;			/* assembly output file */
FILE *tokfile;			/* token output file */

int debug = 0;			/* output debugging info? */
int parse_error = 0;		/* have we had a parse error? */
int error_flag  = 0;		/* have we had a fatal error? */

char
*make_filename (char *base, char *ext)
{
  char *p1 = strrchr (base, '.');
  char *p2 = strrchr (base, '/');
  int rlen = strlen (base);
  int elen = strlen (ext);
  char *new;

  if (p1 && ((p2 == 0) || (p2 < p1)))
    /* if there is a dot and either no slash or the dot is after the last
       slash, then the dot ends the root part of the name */
    rlen = p1 - base;

  new = (char *) malloc (rlen + elen + 2);
  sprintf (new, "%.*s.%s", rlen, base, ext);
  return new;
}

int
main (int argc, char **argv)
{
  int tokenprint = 0;		/* write token file?            */
  int parseprint = 0;		/* print tree after parsing?    */
  int checkprint = 0;		/* print tree after checking?   */
  int allocprint = 0;		/* print tree after allocating? */
  int generate   = 1;		/* generate code 1:generate 0:no-generate */

  char *outname;		/* output file name */
  char *tokname;		/* token output file name */

  ++argv;
  --argc;
  while (argc >= 1 && argv[0][0] == '-') {
    char *flag = *argv++;
    char c;
    --argc;
    while ((c = *++flag))
      switch (c) {
      case 'd': debug      = 1; break;
      case 't': tokenprint = 1; break;
      case 'p': parseprint = 1; break;
      case 'c': checkprint = 1; break;
      case 'a': allocprint = 1; break;
      case 'n': generate   = 0; break;
      default:
        fprintf (stderr, "Unrecognized flag %c ignored\n", c);
      }
  }

  if (argc < 1) {
    fprintf (stderr, "Usage: mpc [-acdnpt] infile [outfile]\n");
    fprintf (stderr, "  infile may omit a trailing .p or .pas\n");
    fprintf (stderr, "  outfile defaults to infile with .spim extension\n");
    fprintf (stderr, "  -d for debugging output\n");
    fprintf (stderr, "  -t for token file (in outfile.token)\n");
    fprintf (stderr, "  -p for AST after parsing (in outfile.parse)\n");
    fprintf (stderr, "  -c for AST after checking (in outfile.check)\n");
    fprintf (stderr, "  -a for AST after allocation (in outfile.alloc)\n");
    fprintf (stderr, "  -n for no code generation\n");
    exit (1);
  }

  /* try name as given, with .p, and with .pas */

  if (!(infile = fopen ((inname = argv[0]                       ), "r")) &&
      !(infile = fopen ((inname = make_filename (argv[0], "p"  )), "r")) &&
      !(infile = fopen ((inname = make_filename (argv[0], "pas")), "r"))) {
    /* complain and quit */
    fprintf (stderr, "Could not open %s, %s, or %s!\n",
	     argv[0], make_filename (argv[0], "p"), inname);
    exit (1);
  }

  --argc;
  ++argv;

  if (argc < 1)
    outname = make_filename (inname, "spim");
  else {
    if (argc > 1) fprintf (stderr, "Extra command line arguments ignored.");
    outname = argv[0];
  }

  if (generate && !(asmout = fopen (outname, "w"))) {
    fprintf (stderr, "Could not open %s for writing!\n", outname);
    exit (1);
  }

  tokfile = 0;
  tokname = make_filename (outname, "token");
  if (tokenprint && !(tokfile = fopen (tokname, "w")))
    fprintf (stderr, "Could not open token file %s; continuing\n", tokname);

  scaninit ();
  parseinit ();

  yyparse ();

  if (parse_error || the_program == 0)
    exit (1);
  if (parseprint)
    print_ast (the_program, make_filename (outname, "parse"));

  check_program (the_program);

  if (checkprint) 
    print_ast (the_program, make_filename (outname, "check"));

  if (error_flag)
    exit (2);

  alloc_fold_program (the_program);

  if (allocprint)
    print_ast (the_program, make_filename (outname, "alloc"));

  if (error_flag)
    exit (3);

  if (generate)
    gen_program (the_program);

  exit (0);
}
