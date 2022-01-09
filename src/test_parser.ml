open Elpi.UnitTests

let _ =
  Parser.program Lexer.token (Lexing.from_string "foo :- bar.")