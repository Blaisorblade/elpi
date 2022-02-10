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
  | App(t,[]) -> clean_app t
  | App(t,ts) as x ->
      let ts' = Util.smart_map clean_app ts in
      let t' = clean_app t in
      if t == t' && ts == ts' then x
      else App(t',ts')
  | Lam(s,t) as x ->
      let t' = clean_app t in
      if t == t' then x else Lam(s,t')
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
| p = pred; FULLSTOP { Program.Pred (snd p, fst p) }
| t = type_; FULLSTOP { Program.Type t }
| KIND; t = kind; FULLSTOP { Program.Type t }
| MODE; m = mode; FULLSTOP { Program.Mode m }
| MACRO; m = macro; FULLSTOP { Program.Macro m }
| CONSTRAINT; cl = list(constant); LCURLY { Program.Constraint(loc $loc, List.map Func.from_string cl) }
| NAMESPACE; c = constant { Program.Namespace(loc $loc, Func.from_string c )}
| SHORTEN; s = shorten; FULLSTOP { Program.Shorten(loc $loc, s) }
| TYPEABBREV; a = typeabbrev; FULLSTOP { Program.TypeAbbreviation a }
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
| attributes = pred_attributes; PRED;
  c = constant; args = separated_list(CONJ,pred_item) { 
   let name = Func.from_string c in
   { Mode.loc=loc $loc; name; args = List.map fst args },
   { Type.loc=loc $loc; attributes; name;
     ty = List.fold_right (fun (_,t) ty ->
       mkApp (loc $loc(c)) [mkCon "->";t;ty]) args (mkCon "prop") }
 }
pred_item:
| io = IO; COLON; ty = ctype_term {
    if io = 'i' then (true, ty)
    else if io = 'o' then (false, ty)
    else assert false
}

kind:
| KIND; names = separated_nonempty_list(CONJ,constant); k = kind_term {
    names |> List.map (fun n->
     { Type.loc=loc $loc; attributes=[]; name=Func.from_string n; ty=k })
  }
type_:
| attributes = type_attributes;
  TYPE; names = separated_nonempty_list(CONJ,constant); t = type_term {
    names |> List.map (fun n->
     { Type.loc=loc $loc; attributes; name=Func.from_string n; ty=t })
  }

ctype_term:
| c = constant { let c = if c = "o" then "prop" else c in
    mkCon c
  }
| hd = type_term; c = constant { let c = if c = "o" then "prop" else c in
    mkApp (loc $loc(hd)) [hd;mkCon c]
  }
| LPAREN; t = ctype_term; RPAREN { t }
type_term:
| t = ctype_term { t }
| hd = ctype_term; ARROW; t = ctype_term { mkApp (loc $loc(hd)) [mkCon "->"; hd; t] }

kind_term:
| TYPE { mkCon "type" }
| hd = TYPE; ARROW; t = kind_term { mkApp (loc $loc(hd)) [mkCon "->"; mkCon "type"; t] }

type_attributes:
| a = pred_attributes { a }

mode:
| { assert false }

macro:
| { assert false }

typeabbrev:
| a = abbrevform; t = type_term {
    let name, args = a in
    let nparams = List.length args in
    let value = List.fold_right mkLam args t in
    { TypeAbbreviation.name = name;
      nparams = nparams;
      value = value;
      loc = loc $loc }
  }

abbrevform:
| c = constant { Func.from_string c, [] }
| LPAREN; hd = constant; args = nonempty_list(constant); RPAREN { Func.from_string hd, args  }


ignored:
| MODULE; constant { Program.Ignored (loc $loc) }
| SIG; constant { Program.Ignored (loc $loc) }

fixity:
| { assert false }

shorten:
| l = trie {
     List.map Func.(fun (x,y) -> from_string x, from_string y) l
  }

trie:
| c = constant; FULLSTOP; LCURLY; l = separated_nonempty_list(CONJ,subtrie); RCURLY {
    List.map (fun (p,x) -> c ^ "." ^ p, x) (List.flatten l)
}
subtrie:
| name = constant { [name,name] }
| prefix = constant; FULLSTOP; LCURLY; l = separated_nonempty_list(CONJ,subtrie); RCURLY {
    List.map (fun (p,x) -> prefix ^ "." ^ p, x) (List.flatten l)
}

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
| EXTERNAL { [Type.External] }
| COLON; l = separated_nonempty_list(COLON, pred_attribute) { l }

pred_attribute:
| PRED_ATTRIBUTE; LPAREN; l = nonempty_list(indexing) ; RPAREN {
    Type.Index l
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
| c = IO { String.make 1 c }