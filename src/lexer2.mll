{
    (* https://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html#s%3Aocamllex-overview
       eventually utf8 one: http://www.cduce.org/ulex/ *)
    open Parser2
    exception Error of string
}

let digit = [ '0' - '9' ]

let pnum = (digit +)

let num = '-' ? pnum

rule linecomment = parse
| '\n' { token lexbuf }
| _ { linecomment lexbuf }

and token = parse
| ( ' ' | '\t' | '\r' | '\n' ) { token lexbuf }
| '%' { linecomment lexbuf }
| "." { FULLSTOP }
| ['a'-'z'] as c { CONSTANT c }
| ":-" { VDASH }
| num as i { INTEGER (int_of_string i) }
| num "." pnum as f { FLOAT (float_of_sting f) }
| '"' string as s { STRING s }
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
