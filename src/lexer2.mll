{
    (* https://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html#s%3Aocamllex-overview
       eventually utf8 one: http://www.cduce.org/ulex/ *)
    open Parser2
    exception Error of string
}

rule token = parse
| "xx" { CONSTANT "xx" }
| ":-" { VDASH }
| "."  { STOP }