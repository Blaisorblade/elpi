open Elpi.UnitTests
type t = Tokens.token =
  | VDASH
  | USEONLY
  | TYPEABBREV
  | TYPE
  | SYMB_TIMES of (string)
  | SYMB_TILDE of (string)
  | SYMB_TICK of (string)
  | SYMB_SLASH of (string)
  | SYMB_SHARP of (string)
  | SYMB_QMARK of (string)
  | SYMB_PLUS of (string)
  | SYMB_MINUS of (string)
  | SYMB_LT of (string)
  | SYMB_GT of (string)
  | SYMB_EXP of (string)
  | SYMB_EQ of (string)
  | SYMB_BTICK of (string)
  | SYMB_AT of (string)
  | SYMB_AND of (string)
  | STRING of ( string )
  | SIGMA
  | SIG
  | SHORTEN
  | RULE
  | RPAREN
  | RCURLY
  | RBRACKET
  | QUOTED of ( string )
  | QDASH
  | PRED_ATTRIBUTE of string
  | PRED
  | PIPE
  | PI
  | OR
  | NAMESPACE
  | MODULE
  | MODE
  | MOD
  | MACRO
  | LPAREN
  | LOCALKIND
  | LOCAL
  | LCURLY
  | LBRACKET
  | KIND
  | IS
  | INTEGER of ( int )
  | IMPORT
  | FULLSTOP
  | FRESHUV
  | FLOAT of ( float )
  | FIXITY
  | EXTERNAL
  | EXPORTDEF
  | EOF
  | DIV
  | DARROW
  | CUT
  | CONSTRAINT
  | CONSTANT of ( string )
  | CONJ
  | COLON
  | CLOSED
  | CLAUSE_ATTRIBUTE of string
  | BIND
  | AS
  | ARROW
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
  test {|"a"|}                        [T(STRING "a", 1, 0, 3)];
  test {|"a""b"|}                     [T(STRING "a\"b", 1, 0, 6)];
  test {|"a\nb"|}                     [T(STRING "a\nb", 1, 0, 6)];
  test {|"a
b"|}                                  [T(STRING "a\nb", 2, 3, 5)];
  (*    01234567890123456789012345 *)
  test  "x"                           [T(CONSTANT "x", 1, 0, 1)];
  test  "x-y"                         [T(CONSTANT "x-y", 1, 0, 3)];
  test  "-y"                          [T(SYMB_MINUS "-y", 1, 0, 2)];
  test  "_y"                          [T(CONSTANT "_y", 1, 0, 2)];
  test  "_X"                          [T(CONSTANT "_X", 1, 0, 2)];
  test  "X_"                          [T(CONSTANT "X_", 1, 0, 2)];
  test  "x?"                          [T(CONSTANT "x?", 1, 0, 2)];
  test  "X"                           [T(CONSTANT "X", 1, 0, 1)];
  test  "X1>@!"                       [T(CONSTANT "X1>@!", 1, 0, 5)];
  test  "a.B.c"                       [T(CONSTANT "a.B.c", 1, 0, 5)];
  test  "a.B."                        [T(CONSTANT "a.B", 1, 0, 3); T(FULLSTOP, 1, 0, 4)];
  test  "a.>"                         [T(CONSTANT "a", 1, 0, 1); T(FULLSTOP, 1, 0, 2); T(SYMB_GT ">", 1, 0, 3)];
  (*    01234567890123456789012345 *)
  test  "-->"                         [T(SYMB_MINUS "-->", 1, 0, 3)];
  test  "x.y->z"                      [T(CONSTANT "x.y->z", 1, 0, 6)];
  (*    01234567890123456789012345 *)
  test  "{{{ }} }}}"                  [T(QUOTED " }} ", 1, 0, 10)];
  test  "{{ {{ } }} }}"               [T(QUOTED " {{ } }} ", 1, 0, 13)];
  test  "{{ x }}3"                    [T(QUOTED " x ", 1, 0, 7); T(INTEGER 3, 1, 0, 8)];
  test  "{{{ x }}}3"                  [T(QUOTED " x ", 1, 0, 9); T(INTEGER 3, 1, 0, 10)];
  test  "{{\n x }}3"                  [T(QUOTED "\n x ", 2, 4, 8); T(INTEGER 3, 2, 4, 9)];
  (*    01234567890123456789012345 *)
  test  "foo :- bar."                 [T(CONSTANT "foo", 1, 0, 3); T(VDASH, 1, 0, 6); T(CONSTANT "bar", 1, 0, 10); T(FULLSTOP, 1, 0, 11)];
  test  "foo :- x \\ bar."            [T(CONSTANT "foo", 1, 0, 3); T(VDASH, 1, 0, 6); T(CONSTANT "x", 1, 0, 8); T(BIND, 1, 0, 10); T(CONSTANT "bar", 1, 0, 14); T(FULLSTOP, 1, 0, 15)];
  test  "foo, bar"                    [T(CONSTANT "foo", 1, 0, 3); T(CONJ, 1, 0, 4); T(CONSTANT "bar", 1, 0, 8) ];
  test  "[]"                          [T(LBRACKET, 1, 0, 1); T(RBRACKET, 1, 0, 2)];
  (*    01234567890123456789012345 *)
  test  "X"                           [T(CONSTANT "X", 1, 0, 1) ];
  test  "is"                          [T(IS, 1, 0, 2) ];
  test  "#line 3 \"xx\"\na"           [T(CONSTANT "a", 3, 0, 1) ];
  test  "b\n#line 3 \"xx\"\na"        [T(CONSTANT "b", 1, 0, 1);T(CONSTANT "a", 3, 2, 1) ];
  test  {|
b
c
#line 7 "xx"
a|}                      [T(CONSTANT "b", 2, 1, 2);T(CONSTANT "c", 3, 3, 4);T(CONSTANT "a", 7, 5, 1) ];
  