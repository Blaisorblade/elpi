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

let ucase = [ 'A' - 'Z' ]
let lcase = [ 'a' - 'z' ]
let schar2 = '+'  | '*' | '/' | '^' | '<' | '>' | '`' | '\'' | '?' | '@' | '#' | '~' | '=' | '&' | '!'
let schar = schar2 | '-' | '$' | '_'
let idchar = lcase | ucase | digit | schar
let idcharstar = idchar *
let idcharstarns = (idchar | "." ( ucase | lcase )) *
let symbchar = lcase | ucase | digit | schar | ':'
let symbcharstar = symbchar *

rule linecomment = parse
| '\n' { new_line lexbuf; token lexbuf }
| _ { skip lexbuf 1; linecomment lexbuf }

and multilinecomment nest = parse
| '\n' { new_line lexbuf; multilinecomment nest lexbuf }
| "*/" { if nest = 0 then token lexbuf else multilinecomment (nest - 1) lexbuf }
| "/*" { multilinecomment (nest+1) lexbuf }
| _ { skip lexbuf 1; multilinecomment nest lexbuf }

and string b = parse
| '\n'     { Buffer.add_char b '\n'; new_line lexbuf; string b lexbuf }
| '\\' 'n' { Buffer.add_char b '\n'; skip lexbuf 2; string b lexbuf }
| '\\' 'b' { Buffer.add_char b '\b'; skip lexbuf 2; string b lexbuf }
| '\\' 't' { Buffer.add_char b '\t'; skip lexbuf 2; string b lexbuf }
| '\\' 'r' { Buffer.add_char b '\r'; skip lexbuf 2; string b lexbuf }
| '\\' '"' { Buffer.add_char b '"';  skip lexbuf 2; string b lexbuf }
| '"' '"'  { Buffer.add_char b '"';  skip lexbuf 2; string b lexbuf }
| '"' { STRING (Buffer.contents b) }
| _ # '"' as c { Buffer.add_char b c; skip lexbuf 1; string b lexbuf }

and quoted n = parse
| '{' { skip lexbuf 1; quoted (n+1) lexbuf }
| '\n' { let b = Buffer.create 80 in Buffer.add_char b '\n'; skip lexbuf 1; new_line lexbuf; quoted_inner b n 0 lexbuf }
| _ as c { let b = Buffer.create 80 in Buffer.add_char b c; skip lexbuf 1; quoted_inner b n 0 lexbuf }

and quoted_inner b n l = parse
| '}' {
    Buffer.add_char b '}'; skip lexbuf 1; 
    try lookahead_close b (n-1) lexbuf;
      if l = 0 then begin
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_p.pos_cnum - 1};
        QUOTED (Buffer.sub b 0 (Buffer.length b -n))
      end
      else quoted_inner b n (l-1) lexbuf
    with Failure _ -> quoted_inner b n l lexbuf
  }
| '{' {
    Buffer.add_char b '{'; skip lexbuf 1; 
    try lookahead_open b (n-1) lexbuf; quoted_inner b n (l+1) lexbuf
    with Failure _ -> quoted_inner b n l lexbuf
  }
| '\n' { Buffer.add_char b '\n'; new_line lexbuf; quoted_inner b n l lexbuf }
| _ as c { Buffer.add_char b c; quoted_inner b n l lexbuf }

and lookahead_close b n = parse
| '}' {
    Buffer.add_char b '}'; skip lexbuf 1; 
    if n = 1 then () else lookahead_close b (n-1) lexbuf
  }

and lookahead_open b n = parse
| '{' {
    Buffer.add_char b '{'; skip lexbuf 1; 
    if n = 1 then () else lookahead_open b (n-1) lexbuf
  }

and token = parse
| ( ' ' | '\t' | '\r' ) { skip lexbuf 1; token lexbuf }
| '\n' { new_line lexbuf; token lexbuf }
| '%' { linecomment lexbuf }
| "/*" { multilinecomment 0 lexbuf }
| "." { FULLSTOP }
| "_" idchar + as c { CONSTANT c }
| "_" { FRESHUV }
| num as i { INTEGER (int_of_string i) }
| num "." pnum as f { FLOAT (float_of_string f) }
| "." pnum as f { FLOAT (float_of_string f) }
| '"' { string (Buffer.create 80) lexbuf }
| ":" { COLON }
| "\\" { BIND }
| "(" { LPAREN }
| ")" { RPAREN }
| "[" { LBRACKET }
| "]" { RBRACKET }
| "{" { LCURLY }
| "}" { RCURLY }
| "|" { PIPE }
| "{{" { skip lexbuf 2; quoted 2 lexbuf }
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
| "as" { AS }
| "is" { IS }
| "->" { ARROW }
| "=>" { DARROW }
| "div" { DIV }
| "mod" { MOD }
| ("infix" | "infixl" | "infixr" | "prefix" | "prefixr" | "postfix" | "postfixl" ) { FIXITY }
| ('+' symbcharstar | "r+" | "i+" | "s+") as s { SYMB_PLUS s }
| ('*' symbcharstar | "r*" | "i*" | "s*") as s { SYMB_TIMES s }
| ('-' symbcharstar | "r-" | "i-" | "s-") as s { SYMB_MINUS s }
| ('<' symbcharstar | "r<" | "i<" | "s<" | "r=<" | "i=<" | "s=<") as s { SYMB_LT s }
| ('>' symbcharstar | "r<" | "i<" | "s>" | "r>=" | "i>=" | "s>=") as s { SYMB_GT s }
| ('~' symbcharstar | "r~" | "i~") as s { SYMB_TILDE s }
| '^' symbcharstar as s { SYMB_EXP s }
| '=' symbcharstar as s { SYMB_EQ s }
| '#' symbcharstar as s { SYMB_SHARP s }
| '&' symbcharstar as s { SYMB_AND s }
| '?' symbcharstar as s { SYMB_QMARK s }
| '@' symbcharstar as s { SYMB_AT s }
| '\'' symbcharstar as s { SYMB_TICK s }
| '`' symbcharstar as s { SYMB_BTICK s }
| '/' symbcharstar as s { SYMB_SLASH s }
| ',' { CONJ }
| ';' { OR }
| "!" { CUT }
| ":-" { VDASH }
| "?-" { QDASH }
| "pi" { PI }
| "sigma" { SIGMA }
| ( "name" | "after" | "before" | "if" | "index" ) as s { CLAUSE_ATTRIBUTE s }
| ( "index" ) as s { PRED_ATTRIBUTE s }
| ucase idcharstar as c { CONSTANT c }
| lcase idcharstarns as c { CONSTANT c }
| eof { EOF }
