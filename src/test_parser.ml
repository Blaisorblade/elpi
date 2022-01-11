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
  let _ = Sys.command (Printf.sprintf "wdiff -t %s %s" f1 f2) in
  exit 1

let mkClause loc attributes body =
  let open Clause in
  Clause { loc; attributes; body }

let mkLoc x y w z =
  { Loc.source_name = ""; source_start = x; source_stop = y; line = w; line_starts_at = z}
  
  
let test s x y w z att b =
  let exp = [mkClause (mkLoc x y w z) att b] in
  let p = Parser.program Lexer.token (Lexing.from_string s) in
  if p <> exp then
    error s p exp

let _ =
  (*    01234567890123456789012345 *)
  test  "foo :- bar." 0 10 1 0 [] (App (mkCon ":-",[mkCon "foo"; mkCon "bar"]));