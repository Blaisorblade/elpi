open Elpi.UnitTests
open Ast
open Ast.Program
open Ast.Term

let error s a1 a2 =
  Printf.eprintf "error parsing '%s':\n%!" s;
  let f1 = Filename.temp_file "parser_out" "txt" in
  let f2 = Filename.temp_file "parser_out" "txt" in
  let oc1 = open_out f1 in
  let oc2 = open_out f2 in
  output_string oc1 (Program.show a1);
  output_string oc2 (Program.show a2);
  close_out oc1;
  close_out oc2;
  let _ = Sys.command (Printf.sprintf "cat %s; cat %s;wdiff -t %s %s" f1 f2 f1 f2) in
  exit 1

let mkClause loc attributes body =
  let open Clause in
  Clause { loc; attributes; body }

let mkLoc x y w z =
  { Loc.source_name = ""; source_start = x; source_stop = y; line = w; line_starts_at = z}
  
  
let test s x y w z att b =
  let exp = [mkClause (mkLoc x y w z) att b] in
  let lexbuf = Lexing.from_string s in
  try
    let p = Parser.program Lexer.token lexbuf in
    if p <> exp then
      error s p exp
    with Parser.Error ->
      Printf.eprintf "error parsing '%s' at char %d\n%!" s lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum;
      exit 1


let (|-) a b = App (mkCon ":-",[a;b])
let (@) a b = App (mkCon a,b)

let (-->) x b = mkLam x b

let c s = mkCon s

let _ =
  (*    01234567890123456789012345 *)
  test  "p :- q."           0 6  1 0 [] (c"p" |- c"q");
  test  "p :- q r."         0 8  1 0 [] (c"p" |- "q" @ [c"r"]);
  test  "p :- x \\ q r."    0 12 1 0 [] (c"p" |- "x" --> ("q" @ [c"r"]));
  test  "p :- q, r."        0 9  1 0 [] (c"p" |- "," @ [c"q";c"r"]);
  test  "p :- q, r, s."     0 12 1 0 [] (c"p" |- "," @ [c"q"; "," @ [c"r";c"s"]]);
  (*    01234567890123456789012345 *)
  test  "p :- q + r * s."   0 14 1 0 [] (c"p" |- "+" @ [c"q"; "*" @ [c"r";c"s"]]);
  test  "p :- q + r , s."   0 14 1 0 [] (c"p" |- "," @ ["+" @ [c"q"; c"r" ]; c"s"]);
  test  "p :- q & r = s."   0 14 1 0 [] (c"p" |- "&" @ [c"q";"=" @ [c"r"; c"s"]]);
  test  "[]."               0 2  1 0 [] (mkNil);
  (*    01234567890123456789012345 *)
  test  "[a,b]."            0 5  1 0 [] (mkSeq [c"a";c"b";mkNil]);
  test  "[(a + b)]."        0 9  1 0 [] (mkSeq ["+" @ [c"a";c"b"];mkNil]);
  test  "[a + b]."          0 7  1 0 [] (mkSeq ["+" @ [c"a";c"b"];mkNil]);
  test  "[f a,b]."          0 7  1 0 [] (mkSeq ["f" @ [c"a"];c"b";mkNil]);
  test  "[(a,b)]."          0 7  1 0 [] (mkSeq ["," @ [c"a";c"b"];mkNil]);
  test  "[a,b|c]."          0 7  1 0 [] (mkSeq [c"a";c"b";c"c"]);
