/* SYMTAB routines and globals */
# include "project.h"

/**********************************************************************/
/*                          Global variables                          */
/**********************************************************************/

SYMTAB *current_symtab   = 0; /* always is top of SYMTAB stack */
DECL   *current_procfunc = 0; /* proc/func being processed now, if any */
int scope_level; /* lexical level:
		    0 = predefined identifiers
		    1 = main program name
		    2 = global declarations
		    3 = args for top level procedures
		    4 = local declarations of top level procedures
		    etc. */
int proc_level;  /* procedure nesting level:
		    0 = predefined identifiers
		    1 = main program (globals)
		    2 = top level procedures
		    etc.

		    Note: this is what text books usually call the "lexical
		    level", but Pascal actually defines two scopes in a
		    procedure, one for the arguments and one for the locals,
		    so we distinguish between scope and procedure nesting
		    levels. */
int symtab_counter;

/**********************************************************************/
/*                         General functions                          */
/**********************************************************************/

SYMTAB *
enter_scope (DECLLIST *decls, int new_plevel)
     /* Make and push a new SYMTAB on the SYMTAB stack. The first argument
	supplies the declarations associated with this scope. The second
	argument indicates whether this is also a new procedure level. */
{
  SYMTAB *new = anew (SYMTAB);

  new->symtab_decls  = decls;
  new->symtab_outer  = current_symtab;
  new->symtab_number = ++symtab_counter;
  new->symtab_level  = ++scope_level;
  new->symtab_plevel = (proc_level += (new_plevel != 0));
  new->symtab_count  = 0;
  new->symtab_offset = (new_plevel ? LINKAGE_SIZE : 0);

  current_symtab = new;

  if (debug)
    printf ("Entering scope %d at level %d, plevel %d\n",
	    symtab_counter, scope_level, proc_level);

  enter_decls (decls);
  return new;
}

void
re_enter_scope (SYMTAB *s)
{
  current_symtab = s;
  symtab_counter = s->symtab_number;
  scope_level    = s->symtab_level;
  proc_level     = s->symtab_plevel;

  if (debug)
    printf ("Entering scope %d at level %d, plevel %d\n",
	    symtab_counter, scope_level, proc_level);

  re_enter_decls (s->symtab_decls);
}

void
exit_scope (void)
{
  SYMTAB *outer = current_symtab->symtab_outer;

  remove_decls (current_symtab->symtab_decls);
  if (debug)
    printf ("Exiting  scope %d at level %d, plevel %d\n",
	    current_symtab->symtab_number,
	    current_symtab->symtab_level,
	    current_symtab->symtab_plevel);
  destroy_temps ();

  assert (outer);
  current_symtab = outer;
  --scope_level;
  assert (scope_level == current_symtab->symtab_level);
  if (proc_level != current_symtab->symtab_plevel)
    --proc_level;
  assert (proc_level == current_symtab->symtab_plevel);
}

void
enter_decl (DECL *d)
{
  IDENT *id = d->decl_id;

  if (check_id_decl (id, d->lineno)) {
    d->decl_symtab = current_symtab;
    d->decl_number = ++(current_symtab->symtab_count);
    d->decl_outer  = id->id_curr_decl;
    id->id_curr_decl = d;
  }
}

void
enter_decls (DECLLIST *dl)
{
  for (; dl; dl = dl->rest)
    enter_decl (dl->this);
}

void
re_enter_decl (DECL *d)
{
  d->decl_id->id_curr_decl = d;
}

void
re_enter_decls (DECLLIST *dl)
{
  for (; dl; dl = dl->rest)
    re_enter_decl (dl->this);
}

void
remove_decl (DECL *d)
{
  if (d->decl_number) {
    /* restores next outer bindings in fast lookup scheme */
    IDENT *id = d->decl_id;
    assert (id->id_curr_decl == d);
    id->id_curr_decl = d->decl_outer;
  }
}

void
remove_decls (DECLLIST *dl)
{
  for (; dl; dl = dl->rest)
    remove_decl (dl->this);
}

BOOLEAN
check_id_decl (IDENT *id, int lineno)
     /* Check if an id may be legally declared within the current scope, i.e.,
	it is not already declared within the scope. Return 1 if OK, otherwise
	print error message and return 0. */
{
  DECL *d;

  if ((d = lookup_id (id)) && d->decl_symtab == current_symtab) {
    errmsg (lineno, "Identifier %s already defined in same scope",
	    id->id_chars);
    return 0;
  }
  if (d) {
    assert (d->decl_symtab);
    if (d->decl_symtab->symtab_plevel == current_symtab->symtab_plevel)
      warning (lineno, "Warning: declaration of %s shadows proc/func formal",
	       id->id_chars);
  }
  return 1;
}

DECL *
lookup_id (IDENT *id)
{
  /* this is coded for the fast lookup scheme */
  /* a slow scheme would have to search symtabs working outwards */
  return id->id_curr_decl;
}

DECL *
check_bind_use (BINDING *b)
     /* Check the use of the identifier in a binding.  If the binding is
	already resolved, return the bound declaration.  Otherwise, obtain the
	current declaration to resolve the binding.  Print an error message if
	there is no current declaration.  Return the appropriate declaration,
	or 0 if there isn't one */
{
  IDENT *id;
  DECL *d;

  if (b == 0)
    return 0;
  else if ((d = b->bind_decl))
    return d;
  else if ((id = b->bind_id) == 0)
    return 0;
  else if ((d = lookup_id (id)))
    return b->bind_decl = d;
  else
    errmsg (b->lineno, "Use of undeclared identifier \"%s\"", id->id_chars);
  return 0;
}

int
alloc_variable (int amount)
     /* Allocate the indicated space for a variable in the current scope;
	return the offset assigned. Update symtab_offset accordingly.  NOTE:
	space is allocated "negatively" on this machine */
{
  int offset;

  assert (current_symtab && current_symtab->symtab_offset <= 0);
  offset = (current_symtab->symtab_offset -= amount);
  if (offset < (-MAX_FRAME_SIZE) && (offset + amount) >= (-MAX_FRAME_SIZE))
    errmsg (current_procfunc ? current_procfunc->lineno : 0,
	    "Frame too large to support variable addressing");
  return offset;
}

int
alloc_formal (int amount)
     /* Allocate the indicated space for a FORMAL in the current scope; return
	the offset assigned.  NOTE: These are allocated "positively", not
	negatively. */
{
  int offset;

  assert (current_symtab);
  offset = current_symtab->symtab_offset;
  assert (offset > 0);
  current_symtab->symtab_offset += amount;
  if (offset <= MAX_FRAME_SIZE &&
      current_symtab->symtab_offset > MAX_FRAME_SIZE)
    errmsg (current_procfunc ? current_procfunc->lineno : 0,
	    "Arguments too large to support addressing");
  return offset;
}
