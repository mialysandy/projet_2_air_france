type token =
  | LABEL of (string)
  | LPAR
  | RPAR
  | COLON
  | COMMA
  | DOLLAR
  | INTPH
  | ADDRPH
  | RPH
  | LPH
  | REGISTER of (ASMType.reg)
  | INTEGER of (int)
  | MOVOP
  | PUSHOP
  | POPOP
  | CALLOP
  | RETOP
  | ADDOP
  | MULOP
  | SUBOP
  | DIVOP
  | XOROP
  | OROP
  | ANDOP
  | NEGOP
  | NOTOP
  | CMPOP
  | TESTOP
  | MINUS
  | SETEOP
  | SETNEOP
  | SETNSOP
  | SETGOP
  | JMPOP
  | JZOP
  | JNZOP
  | LEAOP

val argument :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ASMType.arg
val instruction :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ASMType.instruction
