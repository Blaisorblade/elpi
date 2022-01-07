(* New parser. Doc: http://gallium.inria.fr/~fpottier/menhir/manual.html  *)

%{
open Ast
%}

%token FULLSTOP
%token < string > CONSTANT
%token VDASH
%token < int > INTEGER
%token < float > FLOAT
%token < string > STRING
%token FRESHUV
%token COLON
%token BIND
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LCURLY
%token RCURLY
%token PIPE
%token QUOTED
%token SHORTEN
%token ACCUMULATE
%token LOCAL
%token PRED
%token MODE
%token MACRO
%token RULE
%token NAMESPACE
%token CONSTRAINT
%token KIND
%token TYPE
%token TYPEABBREV
%token EXTERNAL
%token MODULE
%token SIG
%token IMPORT
%token ACCUM_SIG
%token LOCALKIND
%token USEONLY
%token EXPORTDEF
%token CLOSED
%token FIXITY
%token <string> SYMBOL


(* non terminals *)
%type < Program.t > program
%type < Goal.t > goal
%type < (Term.t,Clause.attribute list) Clause.t > clause
%type < Term.t > term

(* entry points *)
%start program
%start goal

%%
program:
| cl = list(clause) {
     List.map (fun x -> Program.Clause x) cl
  }

goal:
| g = term { ( Util.Loc.initial "oops", g ) }

clause:
| hd = term; VDASH; hyps = term; FULLSTOP {
    { Clause.loc = Util.Loc.initial "oops";
      attributes = [];
      body = (Term.App (Term.Const Func.rimplf,[hd;hyps]));
    }
  }

term:
| t = CONSTANT { Term.Const (Func.from_string t) }

list(X):
| { [] }
| hd = X ; tl = list(X) { hd :: tl }
