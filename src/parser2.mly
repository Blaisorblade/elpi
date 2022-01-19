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

let assert_fixity _ = assert false

let rec mkList l stop acc =
  match l with
  | App(Const c,[x;y]) when c == Func.andf -> mkList y stop (x :: acc)
  | t -> mkSeq (List.rev (stop :: t :: acc))

let rec clean_app = function
  | App(t,[]) -> t
  | App(t,ts) -> App(clean_app t,List.map clean_app ts)
  | Lam(s,t) -> Lam(s,clean_app t)
  | x -> x

let underscore () = mkCon "_"

let decode_sequent t =
  match t with
  | App(Const c,[hyps;bo]) when c == Func.sequentf -> hyps, bo
  | _ -> underscore (), t

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
%left SYMB_AT (* 135 *) IFF  
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
| r = chr_rule; FULLSTOP { Program.Chr r }
| p = pred; FULLSTOP { Program.Pred (fst p, snd p) }
| t = type_; FULLSTOP { Program.Type t }
| m = mode; FULLSTOP { Program.Mode m }
| m = macro; FULLSTOP { Program.Macro m }
| CONSTRAINT; cl = list(constant); LCURLY { Program.Constraint(loc $loc, List.map Func.from_string cl) }
| NAMESPACE; c = constant { Program.Namespace(loc $loc, Func.from_string c )}
(*| s = shorten; FULLSTOP { Program.Shorten s }*)
| a = typeabbrev; FULLSTOP { Program.TypeAbbreviation a }
| LCURLY { Program.Begin (loc $loc) }
| RCURLY { Program.End (loc $loc) }
| ignored; FULLSTOP { Program.Ignored (loc $loc) }
| f = fixity; FULLSTOP { assert_fixity f; Program.Ignored (loc $loc)}

chr_rule:
| attributes = chr_rule_attributes; RULE;
  to_match = list(sequent);
  to_remove = preceded(BIND,nonempty_list(sequent))?;
  guard = preceded(PIPE,term)?;
  new_goal = preceded(IFF,sequent)? {
    Chr.create ~to_match ?to_remove ?guard ?new_goal ~attributes ~loc:(loc $loc) ()
  }

pred:
| { assert false }

type_:
| { assert false }

mode:
| { assert false }

macro:
| { assert false }

typeabbrev:
| { assert false }

ignored:
| { assert false }

fixity:
| { assert false }

shorten:
| { assert false }

sequent:
| t = closed_term {
    let context, conclusion = decode_sequent t in
    { Chr.eigen = underscore (); context; conclusion }
  }
| LPAREN; c = constant; COLON; t = term; RPAREN {
    let context, conclusion = decode_sequent t in
    { Chr.eigen = mkCon c; context; conclusion }
  }

goal:
| g = term; EOF { ( loc $loc , g ) }
| g = term; FULLSTOP { ( loc $loc , g ) }

clause:
| attributes = clause_attributes; body = term; {
    { Clause.loc = loc $loc;
      attributes;
      body;
    }
  }
clause_attributes:
| { [] }
| COLON; l = separated_nonempty_list(COLON, clause_attribute) { l }

clause_attribute:
| c = clause_attribute_kwd; s = STRING {
    if c = "name" then Clause.Name s
    else if c = "after" then Clause.After s
    else if c = "before" then Clause.Before s
    else if c = "if" then Clause.If s
    else assert false
  }

chr_rule_attributes:
| { [] }
| COLON; l = separated_nonempty_list(COLON, chr_rule_attribute) { l }

chr_rule_attribute:
| c = chr_rule_attribute_kwd; s = STRING {
    if c = "name" then Chr.Name s
    else if c = "if" then Chr.If s
    else assert false
  }

pred_attributes:
| { [] }
| COLON; l = separated_nonempty_list(COLON, pred_attribute) { l }

pred_attribute:
| PRED_ATTRIBUTE; LPAREN; l = nonempty_list(indexing) ; RPAREN {
    Type.Index s
  }

indexing:
| FRESHUV { 0 }
| i = INTEGER { i }

clause_attribute_kwd:
| x = ATTRIBUTE { x }
| x = CLAUSE_ATTRIBUTE { x }

chr_rule_attribute_kwd:
| x = ATTRIBUTE { x }

term:
| t = term_ { clean_app t }

closed_term:
| t = cterm_ { clean_app t }

term_:
| t = oterm_ { t }
| t = cterm_ { t }

cterm_:
| t = constant { mkCon t }
| x = INTEGER { mkC (cint.Util.CData.cin x)}
| x = FLOAT { mkC (cfloat.Util.CData.cin x)}
| x = STRING { mkC (cstring.Util.CData.cin x)}
| LPAREN; t = term_; RPAREN { App(t,[]) }
| LCURLY; t = term_; RCURLY { App (Const Func.spillf,[t]) }
| LBRACKET; RBRACKET { mkNil }
| LBRACKET; l = term_; RBRACKET { mkList l mkNil [] }
| LBRACKET; l = term_; PIPE; tl = term_; RBRACKET { mkList l tl [] }

oterm_:
| hd = cterm_; arg = cterm_; { mkApp (loc $loc(hd)) [hd ; arg] } %prec OR
| t = constant; BIND; b = term_ { mkLam t b }
| l = term_; s = SYMB_PLUS;  r = term_ { App(mkCon s,[l;r]) }
| l = term_; s = SYMB_TIMES; r = term_ { App(mkCon s,[l;r]) }
| l = term_; s = SYMB_MINUS; r = term_ { App(mkCon s,[l;r]) }
| l = term_; s = SYMB_EXP;   r = term_ { App(mkCon s,[l;r]) }
| l = term_; s = SYMB_LT;    r = term_ { App(mkCon s,[l;r]) }
| l = term_; s = SYMB_GT;    r = term_ { App(mkCon s,[l;r]) }
| l = term_; s = SYMB_EQ;    r = term_ { App(mkCon s,[l;r]) }
| l = term_; s = SYMB_AT;    r = term_ { App(mkCon s,[l;r]) }
| l = term_; s = SYMB_AND;   r = term_ { App(mkCon s,[l;r]) }
| l = term_; s = SYMB_SHARP; r = term_ { App(mkCon s,[l;r]) }
| l = term_; s = SYMB_SLASH; r = term_ { App(mkCon s,[l;r]) }
| l = term_; s = SYMB_BTICK; r = term_ { App(mkCon s,[l;r]) }
| l = term_; s = SYMB_TICK;  r = term_ { App(mkCon s,[l;r]) }
| l = term_; CONJ;  r = term_ { App(mkCon ",",[l;r]) }
| l = term_; OR;    r = term_ { App(mkCon ";",[l;r]) }
| l = term_; AS;    r = term_ { App(mkCon "as",[l;r]) }
| l = term_; IS;    r = term_ { App(mkCon "is",[l;r]) }
| l = term_; MOD;   r = term_ { App(mkCon "mod",[l;r]) }
| l = term_; DIV;   r = term_ { App(mkCon "div",[l;r]) }
| l = term_; ARROW; r = term_ { App(mkCon "->",[l;r]) }
| l = term_; DARROW;r = term_ { App(mkCon "=>",[l;r]) }
| l = term_; VDASH; r = term_ { App(mkCon ":-",[l;r]) }
| l = term_; QDASH; r = term_ { App(mkCon "?-",[l;r]) }
| s = SYMB_TILDE; r = term_ { App(mkCon s,[r]) }
| l = term_; s = SYMB_QMARK; { App(mkCon s,[l]) }

constant:
| c = CONSTANT { c }
| c = CLAUSE_ATTRIBUTE { c }
| c = ATTRIBUTE { c }
| PRED_ATTRIBUTE { "index" }