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

let underscore () = Const (Func.from_string "_")

let mkClause loc attributes body =
  let open Clause in
  Clause { loc; attributes; body }

let mkLoc x y w z =
  { Loc.source_name = ""; source_start = x; source_stop = y; line = w; line_starts_at = z}
  

let chunk s (p1,p2) =
  String.sub s p1.Lexing.pos_cnum (p2.Lexing.pos_cnum - p1.Lexing.pos_cnum)

let message_of_state s = try ParserMessages.message s with Not_found -> "syntax error"
let test s x y w z att b =
  let exp = [mkClause (mkLoc x y w z) att b] in
  let lexbuf = Lexing.from_string s in
  let buffer, lexer = MenhirLib.ErrorReports.wrap Lexer.token in
  try
    let p = Parser.program lexer lexbuf in
    if p <> exp then
      error s p exp
    with Parser.Error stateid ->
      let message = message_of_state stateid in
      let where = MenhirLib.ErrorReports.show (chunk s) buffer in
      Printf.eprintf "%s\nerror parsing '%s' at char %d\n%s%!" where s lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum message;
      exit 1
      
let testR s x y w z attributes to_match to_remove guard new_goal =
  let exp = [Program.Chr (Chr.create ~to_match ~to_remove ?guard ?new_goal ~loc:(mkLoc x y w z) ~attributes ())] in
  let lexbuf = Lexing.from_string s in
  let buffer, lexer = MenhirLib.ErrorReports.wrap Lexer.token in
  try
    let p = Parser.program lexer lexbuf in
    if p <> exp then
      error s p exp
    with Parser.Error stateid ->
      let message = message_of_state stateid in
      let where = MenhirLib.ErrorReports.show (chunk s) buffer in
      Printf.eprintf "%s\nerror parsing '%s' at char %d\n%s%!" where s lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum message;
      exit 1
      
let testF s i msg =
  let lexbuf = Lexing.from_string s in
  try
    let _ = Parser.program Lexer.token lexbuf in
    Printf.eprintf "error, '%s' should not parse\n%!" s;
    exit 1
  with Parser.Error stateid ->
    let message = message_of_state stateid in
    if not @@ Str.string_match (Str.regexp_case_fold msg) message 0 then begin
      Printf.eprintf "warning, '%s' fails with message '%s'\nwhich does not match '%s'\n%!" s message msg;   
      (*exit 1;*)
    end;
    if lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum <> i then begin
      Printf.eprintf "error, '%s' fails at %d instead of %d\n%!" s lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum i;
      exit 1;
    end

let (|-) a b = App (mkCon ":-",[a;b])
let (@) a b = App (mkCon a,b)

let (-->) x b = mkLam x b

let c s = mkCon s

let ss t = { Chr.eigen = underscore (); context = underscore (); conclusion = t }
let s e g t = { Chr.eigen = e; context = g; conclusion = t }

let _ =
  (*    01234567890123456789012345 *)
  test  "p :- q."           0 6  1 0 [] (c"p" |- c"q");
  test  "p :- q r."         0 8  1 0 [] (c"p" |- "q" @ [c"r"]);
  test  "p :- x \\ q r."    0 12 1 0 [] (c"p" |- "x" --> ("q" @ [c"r"]));
  test  "p :- q, r."        0 9  1 0 [] (c"p" |- "," @ [c"q";c"r"]);
  test  "p :- f q, r."      0 11 1 0 [] (c"p" |- "," @ ["f" @ [c"q"];c"r"]);
  test  "p :- q, r, s."     0 12 1 0 [] (c"p" |- "," @ [c"q"; "," @ [c"r";c"s"]]);
  (*    01234567890123456789012345 *)
  test  "p :- q + r * s."   0 14 1 0 [] (c"p" |- "+" @ [c"q"; "*" @ [c"r";c"s"]]);
  test  "p :- q + r , s."   0 14 1 0 [] (c"p" |- "," @ ["+" @ [c"q"; c"r" ]; c"s"]);
  test  "p :- q & r = s."   0 14 1 0 [] (c"p" |- "&" @ [c"q";"=" @ [c"r"; c"s"]]);
  test  "[]."               0 2  1 0 [] (mkNil);
  test  "name."             0 4  1 0 [] (c "name");
  (*    01234567890123456789012345 *)
  test  "[a,b]."            0 5  1 0 [] (mkSeq [c"a";c"b";mkNil]);
  test  "[(a + b)]."        0 9  1 0 [] (mkSeq ["+" @ [c"a";c"b"];mkNil]);
  test  "[a + b]."          0 7  1 0 [] (mkSeq ["+" @ [c"a";c"b"];mkNil]);
  test  "[f a,b]."          0 7  1 0 [] (mkSeq ["f" @ [c"a"];c"b";mkNil]);
  test  "[(a,b)]."          0 7  1 0 [] (mkSeq ["," @ [c"a";c"b"];mkNil]);
  test  "[a,b|c]."          0 7  1 0 [] (mkSeq [c"a";c"b";c"c"]);
  test  "X is a."           0 6  1 0 [] ("is" @ [c"X";c"a"]);
  testF "X is ."            6 ".*expects a right hand side";
  testF "X + ."             5 ".*expects a right hand side";
  testF "X * ."             5 ".*expects a right hand side";
  test  "X is a, Y is b."   0 14 1 0 [] ("," @ ["is" @ [c"X";c"a"];"is" @ [c"Y";c"b"]]);
  (*    01234567890123456789012345 *)
  testF ":-"                2 "unexpected start";
  testF "+"                 1 "unexpected start";
  testF "x. x)"             5 "unexpected ')'";
  testF "x. +"              4 "unexpected start";
  test ":name \"x\" x."     0 11 1 0 [Clause.Name "x"] (c"x");
  (*    01234567890123456789012345 *)
  testF ":nam \"x\" x."     4 "attribute expected";
  testR "rule p (q r)."     0 12 1 0 [] [ss (c"p");ss ("q" @ [c"r"])] [] None None;
  testR "rule (E : G ?- r)."0 17 1 0 [] [s (c "E") (c"G") (c"r")] [] None None;

