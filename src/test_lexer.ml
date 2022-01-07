open Elpi.UnitTests
type t = Tokens.token = 
| VDASH
| USEONLY
| TYPEABBREV
| TYPE
| SYMBOL of (string)
| STRING of ( string )
| SIG
| SHORTEN
| RULE
| RPAREN
| RCURLY
| RBRACKET
| QUOTED
| PRED
| PIPE
| NAMESPACE
| MODULE
| MODE
| MACRO
| LPAREN
| LOCALKIND
| LOCAL
| LCURLY
| LBRACKET
| KIND
| INTEGER of ( int )
| IMPORT
| FULLSTOP
| FRESHUV
| FLOAT of ( float )
| FIXITY
| EXTERNAL
| EXPORTDEF
| CONSTRAINT
| CONSTANT of ( string )
| COLON
| CLOSED
| BIND
| ACCUM_SIG
| ACCUMULATE
[@@deriving show]

let error s n msg =
  Printf.eprintf "lexing '%s' at char %d: %s\n" s n msg;
  exit 1

let validate s (tok1,lnum1,bol1,cnum1) (tok2,lnum2, bol2, cnum2) =
  if tok1 <> tok2 then error s cnum2   (Printf.sprintf "wrong token: got %s instead of %s" (show tok2) (show tok1));
  if lnum1 <> lnum2 then error s cnum2 (Printf.sprintf "wrong line number: got %d instead of %d" lnum2 lnum1);
  if bol1 <> bol2 then error s cnum2   (Printf.sprintf "wrong begin of line: got %d instead of %d" bol2 bol1);
  if cnum1 <> cnum2 then error s cnum2 (Printf.sprintf "wrong char count: got %d instead of %d" cnum2 cnum1)

type exp = T of t * int * int * int | E

let rec expect s b = function
  | [] -> ()
  | sp :: spec ->
      begin try
      let tok2 = Lexer.token b in
      let open Lexing in
      let p = b.lex_curr_p in
      let lnum2, bol2, cnum2 = p.pos_lnum, p.pos_bol, p.pos_cnum in
      match sp with
        | T (tok1,lnum1,bol1,cnum1) -> validate s (tok1,lnum1,bol1,cnum1) (tok2,lnum2, bol2, cnum2)
        | E -> error s cnum2 (Printf.sprintf "wrong lexing: got %s instead of error" (show tok2))
      with Failure _ ->
        match sp with
        | E -> ()
        | T (tok1,_,_,cnum1) -> error s cnum1 (Printf.sprintf "wrong lexing: got error instead of %s" (show tok1))
      end;
      expect s b spec

let test s spec =
  let b = Lexing.from_string s in
  expect s b spec

let () =
  (*    01234567890123456789012345 *)
  test  "3.4"                        [T(FLOAT 3.4, 1, 0, 3)];
  test  " 3.4"                       [T(FLOAT 3.4, 1, 0, 4)];
  test  "\n3.4"                      [T(FLOAT 3.4, 2, 1, 4)];
  test  "3.4 .5"                     [T(FLOAT 3.4, 1, 0, 3); T(FLOAT 0.5, 1, 0, 6)];
  test  "3.4\n .5"                   [T(FLOAT 3.4, 1, 0, 3); T(FLOAT 0.5, 2, 4, 7)];
  (*    01234567890123456789012345 *)
  test  "3 .4"                       [T(INTEGER 3, 1, 0, 1); T(FLOAT 0.4, 1, 0, 4)];
  test  "3..4"                       [T(INTEGER 3, 1, 0, 1); T(FULLSTOP, 1, 0, 2); T(FLOAT 0.4, 1, 0, 4)];
  test  "3."                         [T(INTEGER 3, 1, 0, 1); T(FULLSTOP, 1, 0, 2)];
  test  "-3."                        [T(INTEGER (-3), 1, 0, 2); T(FULLSTOP, 1, 0, 3)];
  (*    01234567890123456789012345 *)
  test  "3%...\n3"                   [T(INTEGER 3, 1, 0, 1); T(INTEGER 3, 2, 6, 7)];
  test  "3/*..*/3"                   [T(INTEGER 3, 1, 0, 1); T(INTEGER 3, 1, 0, 8)];
  test  "3/*\n.*/3"                  [T(INTEGER 3, 1, 0, 1); T(INTEGER 3, 2, 4, 8)];
  test  "3/*\n/*\n*/*/3"             [T(INTEGER 3, 1, 0, 1); T(INTEGER 3, 3, 7, 12)];
  test  "3/*"                        [T(INTEGER 3, 1, 0, 1); E];
  (*    01234567890123456789012345 *)
