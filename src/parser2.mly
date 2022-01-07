(* New parser. Doc: http://gallium.inria.fr/~fpottier/menhir/manual.html  *)

%{
open Ast
open Tokens
%}

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
