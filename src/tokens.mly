%token FULLSTOP
%token < string > CONSTANT
%token VDASH
%token QDASH
%token < int > INTEGER
%token < float > FLOAT
%token < string > STRING
%token FRESHUV
%token CUT
%token COLON
%token BIND
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LCURLY
%token RCURLY
%token PIPE
%token AS
%token IS
%token ARROW
%token DARROW
%token DIV
%token MOD
%token < string > QUOTED
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
%token PI
%token SIGMA
%token < string > CLAUSE_ATTRIBUTE 
%token PRED_ATTRIBUTE 
%token < string > ATTRIBUTE 
%token <string> SYMB_PLUS
%token <string> SYMB_TIMES
%token <string> SYMB_MINUS
%token <string> SYMB_EXP
%token <string> SYMB_LT
%token <string> SYMB_GT
%token <string> SYMB_EQ
%token <string> SYMB_AT
%token <string> SYMB_QMARK
%token <string> SYMB_BTICK
%token <string> SYMB_TICK
%token <string> SYMB_SHARP
%token <string> SYMB_TILDE
%token <string> SYMB_AND
%token CONJ
%token OR
%token IFF
%token <string> SYMB_SLASH
%token EOF

%%