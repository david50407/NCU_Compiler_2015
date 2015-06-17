%{
# include "project.h"

/* forward routines */
void yyerror (char *fmt, ...);
void errmsg (int lineno, char *fmt, ...);
void parseinit ();

static MODEWHICH varmode; /* global variable for var mode propagation */
%}

/* The following %union translates into the union type YYSTYPE in parse.tab.h
   and defines the possible types for elements on bison's semantic stack.
*/
%union {
  int        ival;     /* INTEGER literals */
  TEXT      *tval;     /* string literals (points to text table entry) */
  IDENT     *id;       /* identifiers (points to identifier table entry) */
  PROGRAM   *prog_;
  BLOCK     *block_;
  DECLLIST  *decllist_;
  DECL      *decl_;
  EXPR      *expr_;
  TYPE      *type_;
  STMT      *stmt_;
  STMTLIST  *stmtlist_;
  MODEWHICH  mode_;
  FIELDLIST *fieldlist_;
  CASELIST  *caselist_;
  CASEARM   *casearm_;
  EXPRLIST  *exprlist_;
  BINDLIST  *bindlist_;
}

/* tokens that fundamentally come with values */
%token <ival> INT_
%token <tval> TEXT_
%token <id>   ID_

/* it is helpful if operators come with themselves as values as well */
%token <ival> AND_ DIV_ DOWNTO_ GE_ LE_ MOD_ NE_ NOT_ OR_ TO_
%token <ival> '+' '-' '*' '<' '>' '=' '.' '^' '[' ']'
%token <ival> ',' ':' ';' '(' ')'

/* keywords */
%token ARRAY_ BEGIN_ CASE_ CONST_ DO_ ELSE_ END_ FOR_
%token FUNCTION_ IF_ NIL_ OF_ PROCEDURE_ PROGRAM_
%token RECORD_ REPEAT_ THEN_ TYPE_ UNTIL_ VAR_ WHILE_

/* multi-character operators */
%token <ival> COLON_EQ_ DOT_DOT_

/* non-terminal semantic record types */
%type <ival>        Sign RelOp AddOp MultOp Direction
%type <mode_>       OptVar
%type <prog_>       Program
%type <block_>      Block
%type <decllist_>   OptDefinitionParts DefinitionParts
                    OptConstantDefinitions ConstantDefinitions
                    OptTypeDefinitions TypeDefinitions
                    OptVariableDeclarations VariableDeclaration
                    OptFormalParameters OptParameterGroup ParameterGroup
                    Parameters
%type <decl_>       ConstantDefinition TypeDefinition ProcedureDeclaration
                    FunctionDeclaration
%type <expr_>       Constant UnsignedConstant
                    Expression SimpleExpression Term Factor
                    Variable LimitedVar SubVar
%type <type_>       Type SimpleType EltType
%type <fieldlist_>  FieldList OptFieldList
%type <stmt_>       Statement CompoundStatement IfStatement WhileStatement
                    CaseStatement RepeatStatement ForStatement OptElse
%type <stmtlist_>   Statements
%type <caselist_>   CaseListElements OptCaseListElements
%type <casearm_>    CaseListElement
%type <exprlist_>   CaseLabelList OptExprs
%type <bindlist_>   IdList

%%

/* Compilation Unit Productions */

Program
        : PROGRAM_ ID_ ';' Block '.'
          { DECLLIST *dl = make_decllist (make_program_decl ($2), 0);
	        $$ = make_program (dl, $4);
	      }
        ;

Block
        : OptDefinitionParts BEGIN_ Statements END_
          { 
        /* 
          Feature 1
          Implement the make_block function in astree.c that can create BLOCK node and
          initialize DECLLIST and STMTLIST in this node.
          Hint: You can use anew function find in astree.c to new the BLOCK.
                Read struct.h for more information about struct type.
        */   
            $$ = make_block ($1, $3); 
          }
        ;

OptDefinitionParts
        :
          { $$ = 0; }
        | DefinitionParts
        ;

DefinitionParts
        : CONST_ ConstantDefinitions
          { $$ = $2; }
        | VAR_   VariableDeclaration
          { $$ = $2; }
        | TYPE_  TypeDefinitions
          { $$ = $2; }
        | ProcedureDeclaration OptDefinitionParts
          { $$ = make_decllist ($1, $2); }
        | FunctionDeclaration  OptDefinitionParts
          { $$ = make_decllist ($1, $2); }
        ;

ConstantDefinitions
        : ConstantDefinition OptConstantDefinitions
          { $$ = make_decllist ($1, $2); }
        ;

OptConstantDefinitions
        : ConstantDefinitions
        | OptDefinitionParts
        ;

ConstantDefinition
        : ID_ '=' Constant ';'
          { $$ = make_const_decl ($1, $3); }
        | error ';'
          { $$ = 0; }
        ;

TypeDefinitions
        : TypeDefinition OptTypeDefinitions
          { $$ = make_decllist ($1, $2); }
        ;

OptTypeDefinitions
        : TypeDefinitions
        | OptDefinitionParts
        ;

TypeDefinition
        : ID_ '=' Type ';'
          { $$ = make_type_decl ($1, $3); }
        | error ';'
          { $$ = 0; }
        ;

VariableDeclaration
        : ID_ ':' Type ';' OptVariableDeclarations
        /*
          Feature 2
          Implement the make_var_decl function call in astree.c to initialize AST node.
          Hint: Call make_decl function pre-defined in astree.c to construct DECL node.
        */
          { 
            DECL *d = make_var_decl ($1, $3);
            $$ = make_decllist (d, $5);
          }
        | ID_ ',' VariableDeclaration
          { 
            if ($3 && $3->this && $3->this->tag == DeclVar_) {
	            DECL *d = make_var_decl ($1, $3->this->d.var->var_type);
	            $$ = make_decllist (d, $3);
	        }
            else
              $$ = $3 ;
          }
        | error ';'
          { $$ = 0; }
        ;

OptVariableDeclarations
        : VariableDeclaration
        | OptDefinitionParts
        ;

Constant
        : UnsignedConstant
        | Sign UnsignedConstant
          { $$ = make_un_expr ($1, $2); }
        | TEXT_
          { $$ = make_txt_expr ($1); }
        ;

UnsignedConstant
        : INT_
          { $$ = make_int_expr ($1); }
        | ID_
          { $$ = make_bind_expr ($1); }
        ;

Sign
        : '+' | '-'
        ;

Type
        /*
          Feature 3
          Implement function(s) to handle the following type productions.
          Hint: You will call make_type function pre-defined in astree.c to construct TYPE node.
                Now you need declare and define your own function in astree.h/astree.c.
        */
        : SimpleType
        | ARRAY_ '[' SimpleType EltType
          { $$ = make_array_type ($3, $4); }
        | RECORD_ FieldList END_
          { $$ = make_fieldlist_type ($2); }
        | RECORD_ END_
          { }
        | '^' ID_
          { $$ = make_id_type ($2); }
        | error
          { $$ = the_err_type; }
        ;

EltType
	    : ']' OF_ Type
          { $$ = $3; }
        | ',' SimpleType EltType
        /*
          Feature 3 (continue)
          Call your pre-implemented function here.
        */
          { $$ = make_array_type ($2, $3); }
        ;

SimpleType
        : ID_
          { $$ = make_id_type ($1); }
        | Constant DOT_DOT_ Constant
          { $$ = make_range_type ($1, $3); }
        | '(' IdList ')'
          { $$ = make_enum_type ($2); }
        ;

IdList
        : ID_
          { $$ = make_bindlist (make_binding ($1),  0); }
        | ID_ ',' IdList
          { $$ = make_bindlist (make_binding ($1), $3); }
        ;

FieldList
        /*
          Feature 4
          Implement function(s) to handle the following productions.
          Hint: You can read FIELD and FIELDLIST which pre-defined in struct.h.
        */
        : ID_ ':' Type OptFieldList
          { $$ = make_fieldlist ($1, $3, $4); }
        | ID_ ',' FieldList
          { $$ = make_fieldlist ($1, $3->this->field_type, $3); }
        | error
          { }
        ;

OptFieldList
        :
          { $$ =  0; }
        | ';'
          { $$ =  0; }
        | ';' FieldList
	  { $$ = $2; }

        ;

ProcedureDeclaration
        : PROCEDURE_ ID_ OptFormalParameters ';' Block ';'
	    /*
	      Feature 5 
	      Implement the make_procfunc_decl function to handle this production.
	      Hint: You can read PROCFUNCDEF and DECL which pre-defined in struct.h. 
	    */
          { $$ = make_procfunc_decl ($2, $3, 0, $5); }
        ;

OptFormalParameters
        :
          { $$ =  0; }
        | '(' ParameterGroup ')'
          { $$ = $2; }
        | '(' error ')'
          { $$ =  0; }
        ;

ParameterGroup
        : OptVar
          { varmode = $1; /* this "passes forward" the mode for the group */ }
          Parameters
          { $$ = $3; }
        ;

Parameters
        : ID_ ':' ID_
          { $<decl_>$ = make_formal_decl ($1, varmode, make_id_type ($3)); }
          OptParameterGroup
          { $$ = make_decllist ($<decl_>4, $5); }
        | ID_ ',' Parameters
          { 
            if ($3 && $3->this && $3->this->d.frm) {
              FORMAL *old = $3->this->d.frm;
              DECL *d = make_formal_decl ($1, old->frm_mode, old->frm_type);
              $$ = make_decllist (d, $3);
            }
            else
              $$ = $3;
	      }
        ;

OptParameterGroup
        :
         { $$ = 0; }
        | ';' ParameterGroup
         { $$ = $2; }
        ;

OptVar
        :      { $$ = ModeValue_; }
        | VAR_ { $$ = ModeVar_  ; }
        ;

FunctionDeclaration
        : FUNCTION_ ID_ OptFormalParameters ':' ID_ ';' Block ';'
        /*
          Feature 5 (continue)
          Implement the make_procfunc_decl function to handle this production.
          Hint: You can read PROCFUNCDEF and DECL which pre-defined in struct.h. 
        */
          { $$ = make_procfunc_decl ($2, $3, $5, $7); }
        ;

Statement
        :
          { $$ = 0 ; }
        | Variable COLON_EQ_ Expression
          { $$ = make_assign_stmt ($1, $3); }
        | CompoundStatement
        | IfStatement
        | WhileStatement
        | Variable
          { $$ = make_call_stmt ($1); }
        | CaseStatement
        | RepeatStatement
        | ForStatement
        | error
          { $$ = 0 ; }
        ;

Variable
        : LimitedVar
        | ID_ '(' Expression OptExprs ')'
          { $$ = make_call_expr ($1, make_exprlist ($3, $4)); }
        ;

OptExprs
        :
          { $$ = 0; }
        | ',' Expression OptExprs
          { $$ = make_exprlist ($2, $3); }
        ;

LimitedVar
        : SubVar Expression ']'
	      { $$ = make_bin_expr ('[', $1, $2); }
        | LimitedVar '.' ID_
          { $$ = make_bin_expr ('.', $1, make_id_expr ($3)); }
        | LimitedVar '^'
          { $$ = make_un_expr ('^', $1); }
        | ID_
          { $$ = make_bind_expr ($1); }
        ;

SubVar
        : LimitedVar '['
        | SubVar Expression ','
	      { $$ = make_bin_expr ('[', $1, $2); }
        ;

Expression
        : SimpleExpression
        | SimpleExpression RelOp SimpleExpression
          { $$ = make_bin_expr ($2, $1, $3); }
        ;

RelOp
        : '=' | NE_ | '<' | LE_ | GE_ | '>'
        ;

SimpleExpression
        : Term
        | Sign Term
          { $$ = make_un_expr ($1, $2); }
        | SimpleExpression AddOp Term
          { $$ = make_bin_expr ($2, $1, $3); }
        ;

AddOp
        : '+' | '-' | OR_
        ;

Term
        : Factor
        | Term MultOp Factor
          { $$ = make_bin_expr ($2, $1, $3); }
        ;

MultOp
        : '*' | DIV_ | MOD_ | AND_
        ;

Factor
        : Variable
        | '(' Expression ')'
          { $$ = $2; }
        | '(' error ')'
          { $$ = 0; }
        | NOT_ Factor
          { $$ = make_un_expr (NOT_, $2); }
        | INT_
          { $$ = make_int_expr ($1); }
        | TEXT_
          { $$ = make_txt_expr ($1); }
        | NIL_
          { $$ = the_nil_expr; }
        ;

CompoundStatement
        : BEGIN_ Statements END_
        /*
          Feature 6
          Implement the function in astree.c to handle this production.
        */
          { $$ = make_compound_stmt ($2); }
        ;

Statements
        : Statement
        /*
          Feature 6 (continue)
          Implement the function in astree.c to handle this production.
        */
          { $$ = make_stmtlist ($1, 0); }
        | Statement ';' Statements
        /*
          Feature 6 (continue)
          Implement the function in astree.c to handle this production.
        */
          { $$ = make_stmtlist ($1, $3); }
        ;

IfStatement
        : IF_ Expression THEN_ Statement OptElse
        /*
          Feature 7
          Implement the function in astree.c to handle this production.
        */
          { $$ = make_ifstmt ($2, $4, $5); }
        ;

OptElse
        :
          { $$ =  0; }
        | ELSE_ Statement
          { $$ = $2; }
        ;

CaseStatement
        : CASE_ Expression OF_ CaseListElements END_
        /*
          Feature 8
          Fill the following rule action and implement the function if you need
          in astree.c to handle this production.
        */
          { }
        ;

CaseListElements
        : CaseListElement OptCaseListElements
        /*
          Feature 8 (continue)
          Fill the following rule action and implement the function in astree.c 
          if you need.
        */
          { } 
        ;

OptCaseListElements
        /*
          Feature 8 (continue)
          Fill the following rule action and implement the function in astree.c 
          if you need.
        */
        :
          { }
        | ';'
          { }
        | ';' CaseListElements
          { }
        ;

CaseListElement
        /*
          Feature 8 (continue)
          Fill the following rule action and implement the function in astree.c 
          if you need.
        */
        : CaseLabelList ':' Statement
          { }
        | error
          { }
        ;

CaseLabelList
        : Constant
          { $$ = make_exprlist ($1,  0); }
        | Constant ',' CaseLabelList
          { $$ = make_exprlist ($1, $3); }
        ;

WhileStatement
        : WHILE_ Expression DO_ Statement
         { $$ = make_whilestmt ($2, $4); }
        ;

RepeatStatement
        : REPEAT_ Statements UNTIL_ Expression
        /*
          Feature 9
          Fill the following rule action and implement the function in astree.c 
          if you need.
        */
         { }
        ;

ForStatement
        : FOR_ ID_ COLON_EQ_ Expression Direction Expression
          DO_ Statement
        /*
          Feature 10
          Fill the following rule action and implement the function in astree.c 
          if you need.
        */
         { }
        ;

Direction
        : TO_ | DOWNTO_
        ;

/*
  Feature 11
  Go to the gen_code.c you will find there are a lot of pre-defined function to generate the
  corresponding assembly code (This assembly language is MIPS).
  So, when you execute (./mpc input.pas output.s) command in our linux system, you will got a 
  output.s file for MIPS assembler.
  Now you have to translate the original MIPS version to NASM version.
  (The grammar of NASM is similar to MASM, you can search NASM tutorial for MASM user from
  internet help you getting started quickly.)
  Finally, try to compile output.s to got an excutable program.

  Hint: You need to modify tempreg.c , gen_code.c.
  
  Hint: How to use nasm to generate executable program?

        (In your project directory)
        (Use nasm to generate *.o file)
        nasm -f elf my_pascal_test.s

        (Use ld to link *.o files and library and option -o for output file)
        ld -m elf_i386 my_pascal_test.o lib/libasm.a -o my_pascal_test

  Hint: We have pre-defined a lot of useful MASM like function to let you write assembly easily.
        You can see these pre-defined MASM like function declared in lib/libasm.h, such as
        Write, WriteInt and WriteString...etc
        NASM assembly program usually begin with _start, but when you include our libasm.h
        you don't need to implement _start Label code. We will auto generate the content of 
        _start to call your main label.
        So, remember to add %include "libasm.h" at the begining of assembly code.
*/

%%

void
yyerror (char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  errmsg (yylineno, fmt, args);
  va_end (args);
  parse_error = 1;
}

void
warning (int lineno, char *fmt, ...)
{
  va_list args;
  fprintf (stderr, "%s:%d: ", inname, lineno);
  va_start (args, fmt);
  vfprintf (stderr, fmt, args);
  va_end (args);
  fprintf (stderr, "\n");
}

void
errmsg (int lineno, char *fmt, ...)
/* Use of <stdargs> allows a call to yyerror to specify a format string
   followed by multiple arguments */
{
  va_list args;
  /* Print filename and line number at beginning of every error message.
     Use same format as gcc produces, so can compile and check errors
     within emacs. */
  fprintf (stderr, "%s:%d: ", inname, lineno);
  /* Print specific error message */
  va_start (args, fmt);
  vfprintf (stderr, fmt, args);
  va_end (args);
  fprintf (stderr, "\n");
  error_flag = 1;
}

void
parseinit ()
{
  init_types ();
  init_objects ();
  init_exprs ();
  init_decls ();
  init_regtable ();
}
