(* New parser. Doc: http://gallium.inria.fr/~fpottier/menhir/manual.html  *)

%{
open Ast
open Tokens
open Term

let loc (startpos, endpos) = {
  Util.Loc.source_name = startpos.Lexing.pos_fname;
  source_start = startpos.Lexing.pos_cnum;
  source_stop = endpos.Lexing.pos_cnum;
  line = startpos.Lexing.pos_lnum;
  line_starts_at = startpos.Lexing.pos_bol;
}

let desugar_multi_binder loc = function
  | App(Const hd as binder,args)
    when Func.equal hd Func.pif || Func.equal hd Func.sigmaf && List.length args > 1 ->
      let last, rev_rest = let l = List.rev args in List.hd l, List.tl l in
      let names = List.map (function
        | Const x -> Func.show x
        | (App _ | Lam _ | CData _ | Quoted _) ->
            failwith "multi binder syntax") rev_rest in
      let body = mkApp loc [binder;last] in
      List.fold_left (fun bo name -> mkApp loc [binder;mkLam name bo]) body names
  | (App _ | Const _ | Lam _ | CData _ | Quoted _) as t -> t
;;

%}

%on_error_reduce term

(* non terminals *)
%type < Program.t > program
%type < Goal.t > goal
%type < (Term.t,Clause.attribute list) Clause.t > clause
%type < Term.t > term
%type < Program.decl > decl

(* entry points *)
%start program
%start goal

%left AS          (* 0   *)
%nonassoc VDASH   (* 1   *) QDASH (* was 115 ?? *)
%right OR         (* 100 *)
%right CONJ       (* 110 *)
%right ARROW      (* 116 *)
%right DARROW     (* 129 *)
%nonassoc SYMB_TICK 
%left SYMB_AND    (* 128 *) 
%nonassoc SYMB_EQ (* 130 *) SYMB_LT SYMB_GT IS   
%left SYMB_AT (* 135 *)   
%nonassoc SYMB_BTICK (* 141 *)
%left SYMB_EXP (* 150 *) SYMB_PLUS SYMB_MINUS
%left SYMB_TIMES (* 160 *) SYMB_SLASH DIV MOD
%nonassoc SYMB_TILDE (* 256 *)
%left SYMB_SHARP (* ??? *)
%right BIND
%nonassoc CONSTANT INTEGER FLOAT STRING LPAREN LCURLY LBRACKET


%%
program:
| EOF { [] }
| d = decl; p = program { d :: p }

decl:
| c = clause; FULLSTOP { Program.Clause c }

goal:
| g = term; EOF { ( loc $loc , g ) }

clause:
| body = term; {
    { Clause.loc = loc $loc;
      attributes = [];
      body;
    }
  }


term:
| t = oterm { t }
| t = cterm { t }

cterm:
| t = CONSTANT { mkCon t }
| x = INTEGER { mkC (cint.Util.CData.cin x)}
| x = FLOAT { mkC (cfloat.Util.CData.cin x)}
| x = STRING { mkC (cstring.Util.CData.cin x)}
| LPAREN; t = term; RPAREN { t }
| LCURLY; t = term; RCURLY { App (Const Func.spillf,[t]) }
| LBRACKET; l = separated_list(CONJ,cterm); RBRACKET { mkSeq (l @ [mkNil]) }
| LBRACKET; l = oterm; RBRACKET { mkSeq [l;mkNil] }
| LBRACKET; l = separated_list(CONJ,cterm); PIPE; tl = term; RBRACKET { mkSeq (l @[tl]) }
| hd = cterm; arg = cterm; { mkApp (loc $loc(hd)) [hd ; arg] } %prec CONSTANT

oterm:
| t = CONSTANT; BIND; b = term { mkLam t b }
| l = term; s = SYMB_PLUS;  r = term { App(mkCon s,[l;r]) }
| l = term; s = SYMB_TIMES; r = term { App(mkCon s,[l;r]) }
| l = term; s = SYMB_MINUS; r = term { App(mkCon s,[l;r]) }
| l = term; s = SYMB_EXP;   r = term { App(mkCon s,[l;r]) }
| l = term; s = SYMB_LT;    r = term { App(mkCon s,[l;r]) }
| l = term; s = SYMB_GT;    r = term { App(mkCon s,[l;r]) }
| l = term; s = SYMB_EQ;    r = term { App(mkCon s,[l;r]) }
| l = term; s = SYMB_AT;    r = term { App(mkCon s,[l;r]) }
| l = term; s = SYMB_AND;   r = term { App(mkCon s,[l;r]) }
| l = term; s = SYMB_SHARP; r = term { App(mkCon s,[l;r]) }
| l = term; s = SYMB_SLASH; r = term { App(mkCon s,[l;r]) }
| l = term; s = SYMB_BTICK; r = term { App(mkCon s,[l;r]) }
| l = term; s = SYMB_TICK;  r = term { App(mkCon s,[l;r]) }
| l = term; CONJ;  r = term { App(mkCon ",",[l;r]) }
| l = term; OR;    r = term { App(mkCon ";",[l;r]) }
| l = term; AS;    r = term { App(mkCon "as",[l;r]) }
| l = term; IS;    r = term { App(mkCon "is",[l;r]) }
| l = term; MOD;   r = term { App(mkCon "mod",[l;r]) }
| l = term; DIV;   r = term { App(mkCon "div",[l;r]) }
| l = term; ARROW; r = term { App(mkCon "->",[l;r]) }
| l = term; DARROW;r = term { App(mkCon "=>",[l;r]) }
| l = term; VDASH; r = term { App(mkCon ":-",[l;r]) }
| l = term; QDASH; r = term { App(mkCon "?-",[l;r]) }
| s = SYMB_TILDE; r = term { App(mkCon s,[r]) }
| l = term; s = SYMB_QMARK; { App(mkCon s,[l]) }
