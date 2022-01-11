(* New parser. Doc: http://gallium.inria.fr/~fpottier/menhir/manual.html  *)

%{
open Ast
open Tokens

let loc (startpos, endpos) = {
  Util.Loc.source_name = startpos.Lexing.pos_fname;
  source_start = startpos.Lexing.pos_cnum;
  source_stop = endpos.Lexing.pos_cnum;
  line = startpos.Lexing.pos_lnum;
  line_starts_at = startpos.Lexing.pos_bol;
}

%}

(* non terminals *)
%type < Program.t > program
%type < Goal.t > goal
%type < (Term.t,Clause.attribute list) Clause.t > clause
%type < Term.t > term
%type < Program.decl > decl

(* entry points *)
%start program
%start goal

%nonassoc VDASH CONSTANT INTEGER FLOAT STRING
%nonassoc LPAREN
%left SYMB_PLUS
%right BIND


%%
program:
| EOF { [] }
| d = decl; p = program { d :: p }

decl:
| c = clause; FULLSTOP { Program.Clause c }

goal:
| g = term; EOF { ( Util.Loc.initial "oops", g ) }

clause:
| hd = term; VDASH; hyps = term {
    { Clause.loc = loc $loc;
      attributes = [];
      body = (Term.App (Term.Const Func.rimplf,[hd;hyps]));
    }
  }


term:
| t = CONSTANT { Term.mkCon t }
(*| t = CONSTANT; BIND; b = term { Term.mkLam t b }*)
| x = INTEGER { Term.mkC (cint.Util.CData.cin x)}
| x = FLOAT { Term.mkC (cfloat.Util.CData.cin x)}
| x = STRING { Term.mkC (cstring.Util.CData.cin x)}
| a = term; VDASH; b = term {Term.(mkApp (loc $loc(a)) [mkCon ":-";a;b]) } 
| hd = term; arg = term; { Term.mkApp (loc $loc(hd)) [hd ; arg] } %prec BIND
| LPAREN; t = term; RPAREN { t }
