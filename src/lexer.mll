{
    (* https://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html#s%3Aocamllex-overview
       eventually utf8 one: http://www.cduce.org/ulex/ *)
    open Tokens
    exception Error of string

    let new_line b =
      Lexing.new_line b

    let skip b n =
      let open Lexing in
      b.lex_curr_p <- { b.lex_curr_p with pos_cnum = b.lex_curr_p.pos_cnum + n }

}

let digit = [ '0' - '9' ]

let pnum = (digit +)

let num = '-' ? pnum

let stringchar = _ # '"'

let string = ( '"' '"' | stringchar) * '"'

rule linecomment = parse
| '\n' { new_line lexbuf; token lexbuf }
| _ { skip lexbuf 1; linecomment lexbuf }

and multilinecomment nest = parse
| '\n' { new_line lexbuf; multilinecomment nest lexbuf }
| "*/" { if nest = 0 then token lexbuf else multilinecomment (nest - 1) lexbuf }
| "/*" { multilinecomment (nest+1) lexbuf }
| _ { skip lexbuf 1; multilinecomment nest lexbuf }

and string b = parse
| '\n' as c { new_line lexbuf; Buffer.add_char b c; string b lexbuf }
| '"' { STRING (Buffer.contents b) }
| _ # '"' as c { Buffer.add_char b c; skip lexbuf 1; string b lexbuf }

and token = parse
| ( ' ' | '\t' | '\r' ) { skip lexbuf 1; token lexbuf }
| '\n' { new_line lexbuf; token lexbuf }
| '%' { linecomment lexbuf }
| "/*" { multilinecomment 0 lexbuf }
| "." { FULLSTOP }
| ['a'-'z'] + as c { CONSTANT c }
| ":-" { VDASH }
| num as i { INTEGER (int_of_string i) }
| num "." pnum as f { FLOAT (float_of_string f) }
| "." pnum as f { FLOAT (float_of_string f) }
| '"' { string (Buffer.create 80) lexbuf }
| "_" { FRESHUV }
| ":" { COLON }
| "\\" { BIND }
| "(" { LPAREN }
| ")" { RPAREN }
| "[" { LBRACKET }
| "]" { RBRACKET }
| "{" { LCURLY }
| "}" { RCURLY }
| "|" { PIPE }
| "" { QUOTED }
| "shorten" { SHORTEN }
| "accumulate" { ACCUMULATE }
| "local" { LOCAL }
| "pred" { PRED }
| "mode" { MODE }
| "macro" { MACRO }
| "rule" { RULE }
| "namespace" { NAMESPACE }
| "constraint" { CONSTRAINT }
| "kind" { KIND }
| "type" { TYPE }
| "typeabbrev" { TYPEABBREV }
| "external" { EXTERNAL }
| "module" { MODULE }
| "sig" { SIG }
| "import" { IMPORT }
| "accum_sig" { ACCUM_SIG }
| "localkind" { LOCALKIND }
| "useonly" { USEONLY }
| "exportdef" { EXPORTDEF }
| "closed" { CLOSED }
| ("infix" | "infixl" | "infixr" | "prefix" | "prefixr" | "postfix" | "postfixl" ) { FIXITY }
