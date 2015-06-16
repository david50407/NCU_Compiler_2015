# include "project.h"

/* -------- routines supporting code generation ---------- */

void gen (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  vfprintf (asmout, fmt, args);
  va_end (args);
}

void gen_inst (const char *fmt, ...)
{
  va_list args;
  char c;

  putc ('\t', asmout);
  va_start (args, fmt);
  while ((c = *fmt++))
    switch (c) {
    case '#':
      fprintf (asmout, "%d", va_arg (args, int));
      break;
    case '@':
      fprintf (asmout, "L%d", va_arg (args, int));
      break;
    case '~':
      fprintf (asmout, "%s", va_arg (args, char *));
      break;
    case '%':
      fprintf (asmout, "%s", reg_pname[va_arg (args, int)]);
      break;
    default:
      putc (c, asmout);
    }
  va_end (args);
  putc ('\n', asmout);
}

void gen_text_constant (TEXT *txt)
{
  int n = strlen (txt->text_chars);
  char *p = txt->text_chars;
  char c;

  gen ("LS%d:\t.asciiz \"", txt->text_number);
  while (--n >= 0) {
    c = *p++;
    switch (c) {
    case '\n': gen ("\\n"); break;
    case '\t': gen ("\\t"); break;
    case '\b': gen ("\\b"); break;
    case '\r': gen ("\\r"); break;
    case '\f': gen ("\\f"); break;
    case '\\': gen ("\\"); break;
    case '\"': gen ("\\\""); break;
    default:
      if (c < ' ' || c > '\176' || c == '\"')
	gen ("\\%d%d%d", c >> 6, (c >> 3) & 7, c & 7);
      else
	putc (c, asmout);
      break;
    }
  }
  gen ("\"\n");
}

void gen_label (int lab)
{
  gen ("L%d:\n", lab);
}
