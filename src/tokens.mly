%token FULLSTOP
%token < string > CONSTANT
%token VDASH
%token < int > INTEGER
%token < float > FLOAT
%token < string > STRING
%token FRESHUV
%token COLON
%token BIND
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LCURLY
%token RCURLY
%token PIPE
%token QUOTED
%token SHORTEN
%token ACCUMULATE
%token LOCAL
%token PRED
%token MODE
%token MACRO
%token RULE
%token NAMESPACE
%token CONSTRAINT
%token KIND
%token TYPE
%token TYPEABBREV
%token EXTERNAL
%token MODULE
%token SIG
%token IMPORT
%token ACCUM_SIG
%token LOCALKIND
%token USEONLY
%token EXPORTDEF
%token CLOSED
%token FIXITY
%token <string> SYMBOL

%%