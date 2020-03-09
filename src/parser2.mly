(* New parser. Doc: http://gallium.inria.fr/~fpottier/menhir/manual.html  *)

%{
open Ast
%}

%token VDASH
%token STOP
%token < string > CONSTANT


(* non terminals *)
%type < Ast.Structured.program > program
%type < Ast.Goal.t > goal
%type < (Term.t,Structured.attribute) Clause.t > clause
%type < Term.t > term

(* entry points *)
%start program
%start goal

%%
program:
| cl = list(clause) {
    { Structured.macros = [];
      types = [];
      type_abbrevs = [];
      modes = [];
      body = [Clauses cl];
    }
  }

goal:
| g = term { ( Util.Loc.initial "oops", g ) }

clause:
| hd = term; VDASH; hyps = term; STOP {
    { Clause.loc = Util.Loc.initial "oops";
      attributes = { Structured.insertion = None; ifexpr = None; id = None };
      body = (Term.App (Term.Const Func.rimplf,[hd;hyps]));
    }
  }

term:
| t = CONSTANT { Term.Const (Func.from_string t) }

list(X):
| { [] }
| hd = X ; tl = list(X) { hd :: tl }
