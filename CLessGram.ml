type token =
  | IDENTIFIER of (string)
  | TYPE_NAME of (string)
  | CONSTANT of (int)
  | STRING_LITERAL of (string)
  | SIZEOF
  | PTR_OP
  | INC_OP
  | DEC_OP
  | LEFT_OP
  | RIGHT_OP
  | LE_OP
  | GE_OP
  | EQ_OP
  | NE_OP
  | AND_OP
  | OR_OP
  | MUL_ASSIGN
  | DIV_ASSIGN
  | MOD_ASSIGN
  | ADD_ASSIGN
  | SUB_ASSIGN
  | LEFT_ASSIGN
  | RIGHT_ASSIGN
  | AND_ASSIGN
  | XOR_ASSIGN
  | OR_ASSIGN
  | SEMI_CHR
  | OPEN_BRACE_CHR
  | CLOSE_BRACE_CHR
  | COMMA_CHR
  | COLON_CHR
  | EQ_CHR
  | OPEN_PAREN_CHR
  | CLOSE_PAREN_CHR
  | OPEN_BRACKET_CHR
  | CLOSE_BRACKET_CHR
  | DOT_CHR
  | AND_CHR
  | OR_CHR
  | XOR_CHR
  | BANG_CHR
  | TILDE_CHR
  | ADD_CHR
  | SUB_CHR
  | STAR_CHR
  | DIV_CHR
  | MOD_CHR
  | OPEN_ANGLE_CHR
  | CLOSE_ANGLE_CHR
  | QUES_CHR
  | TYPEDEF
  | EXTERN
  | STATIC
  | AUTO
  | REGISTER
  | CHAR
  | SHORT
  | INTEGER
  | LONG
  | SIGNED
  | UNSIGNED
  | FLOATING
  | DOUBLE
  | CONST
  | VOLATILE
  | VOID
  | STRUCT
  | UNION
  | ENUM
  | ELLIPSIS
  | EOF
  | CASE
  | DEFAULT
  | IF
  | ELSE
  | SWITCH
  | WHILE
  | DO
  | FOR
  | GOTO
  | CONTINUE
  | BREAK
  | RETURN
  | ASM

open Parsing;;
let _ = parse_error;;
# 2 "CLessGram.mly"

(*
 *	Copyright (c) 2005 by Laboratoire Spécification et Vérification (LSV),
 *	UMR 8643 CNRS & ENS Cachan.
 *	Written by Jean Goubault-Larrecq.  Derived from the csur project.
 *
 *	Permission is granted to anyone to use this software for any
 *	purpose on any computer system, and to redistribute it freely,
 *	subject to the following restrictions:
 *
 *	1. Neither the author nor its employer is responsible for the consequences of use of
 *		this software, no matter how awful, even if they arise
 *		from defects in it.
 *
 *	2. The origin of this software must not be misrepresented, either
 *		by explicit claim or by omission.
 *
 *	3. Altered versions must be plainly marked as such, and must not
 *		be misrepresented as being the original software.
 *
 *	4. This software is restricted to non-commercial use only.  Commercial
 *		use is subject to a specific license, obtainable from LSV.
 *
*)

(*
        Modified version by Benoit Barbot 2016
*)

(* Analyse syntaxique d'un sous-ensemble (tres) reduit de C.
 *)

open CLessType

let get_index ()= Index (* raise Parse_error*)
let get_setArray (x,y,z) = SetArray (x,y,z) (*raise Parse_error*)
let get_reference x = Ref x (*raise Parse_error (* Ref x *)*)
let get_dereference () = (*raise Parse_error*) Deref
let get_setReference () = (*raise Parse_error *) SetReference

# 131 "CLessGram.ml"
let yytransl_const = [|
  261 (* SIZEOF *);
  262 (* PTR_OP *);
  263 (* INC_OP *);
  264 (* DEC_OP *);
  265 (* LEFT_OP *);
  266 (* RIGHT_OP *);
  267 (* LE_OP *);
  268 (* GE_OP *);
  269 (* EQ_OP *);
  270 (* NE_OP *);
  271 (* AND_OP *);
  272 (* OR_OP *);
  273 (* MUL_ASSIGN *);
  274 (* DIV_ASSIGN *);
  275 (* MOD_ASSIGN *);
  276 (* ADD_ASSIGN *);
  277 (* SUB_ASSIGN *);
  278 (* LEFT_ASSIGN *);
  279 (* RIGHT_ASSIGN *);
  280 (* AND_ASSIGN *);
  281 (* XOR_ASSIGN *);
  282 (* OR_ASSIGN *);
  283 (* SEMI_CHR *);
  284 (* OPEN_BRACE_CHR *);
  285 (* CLOSE_BRACE_CHR *);
  286 (* COMMA_CHR *);
  287 (* COLON_CHR *);
  288 (* EQ_CHR *);
  289 (* OPEN_PAREN_CHR *);
  290 (* CLOSE_PAREN_CHR *);
  291 (* OPEN_BRACKET_CHR *);
  292 (* CLOSE_BRACKET_CHR *);
  293 (* DOT_CHR *);
  294 (* AND_CHR *);
  295 (* OR_CHR *);
  296 (* XOR_CHR *);
  297 (* BANG_CHR *);
  298 (* TILDE_CHR *);
  299 (* ADD_CHR *);
  300 (* SUB_CHR *);
  301 (* STAR_CHR *);
  302 (* DIV_CHR *);
  303 (* MOD_CHR *);
  304 (* OPEN_ANGLE_CHR *);
  305 (* CLOSE_ANGLE_CHR *);
  306 (* QUES_CHR *);
  307 (* TYPEDEF *);
  308 (* EXTERN *);
  309 (* STATIC *);
  310 (* AUTO *);
  311 (* REGISTER *);
  312 (* CHAR *);
  313 (* SHORT *);
  314 (* INTEGER *);
  315 (* LONG *);
  316 (* SIGNED *);
  317 (* UNSIGNED *);
  318 (* FLOATING *);
  319 (* DOUBLE *);
  320 (* CONST *);
  321 (* VOLATILE *);
  322 (* VOID *);
  323 (* STRUCT *);
  324 (* UNION *);
  325 (* ENUM *);
  326 (* ELLIPSIS *);
    0 (* EOF *);
  327 (* CASE *);
  328 (* DEFAULT *);
  329 (* IF *);
  330 (* ELSE *);
  331 (* SWITCH *);
  332 (* WHILE *);
  333 (* DO *);
  334 (* FOR *);
  335 (* GOTO *);
  336 (* CONTINUE *);
  337 (* BREAK *);
  338 (* RETURN *);
  339 (* ASM *);
    0|]

let yytransl_block = [|
  257 (* IDENTIFIER *);
  258 (* TYPE_NAME *);
  259 (* CONSTANT *);
  260 (* STRING_LITERAL *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\002\000\002\000\004\000\005\000\005\000\
\003\000\007\000\008\000\009\000\009\000\009\000\009\000\009\000\
\009\000\011\000\011\000\013\000\013\000\013\000\013\000\014\000\
\014\000\014\000\014\000\014\000\016\000\017\000\018\000\019\000\
\020\000\010\000\015\000\021\000\021\000\021\000\021\000\022\000\
\022\000\022\000\023\000\024\000\024\000\024\000\024\000\024\000\
\025\000\025\000\025\000\026\000\027\000\028\000\029\000\029\000\
\030\000\030\000\012\000\012\000\006\000\006\000\031\000\033\000\
\033\000\034\000\034\000\035\000\036\000\032\000\032\000\032\000\
\032\000\037\000\037\000\037\000\037\000\037\000\043\000\044\000\
\038\000\038\000\038\000\038\000\046\000\046\000\045\000\045\000\
\039\000\039\000\047\000\048\000\040\000\040\000\049\000\050\000\
\041\000\041\000\041\000\051\000\042\000\042\000\001\000\001\000\
\001\000\053\000\053\000\052\000\052\000\055\000\056\000\056\000\
\057\000\057\000\058\000\058\000\059\000\054\000\000\000"

let yylen = "\002\000\
\001\000\002\000\001\000\001\000\003\000\001\000\001\000\002\000\
\001\000\001\000\001\000\001\000\004\000\003\000\004\000\002\000\
\002\000\001\000\003\000\001\000\002\000\002\000\002\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\003\000\003\000\003\000\001\000\
\003\000\003\000\001\000\001\000\003\000\003\000\003\000\003\000\
\001\000\003\000\003\000\001\000\001\000\001\000\001\000\003\000\
\001\000\003\000\001\000\003\000\001\000\003\000\003\000\000\000\
\001\000\001\000\003\000\001\000\001\000\001\000\001\000\002\000\
\002\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\002\000\003\000\003\000\004\000\001\000\002\000\001\000\002\000\
\001\000\002\000\001\000\001\000\005\000\007\000\001\000\001\000\
\005\000\006\000\007\000\001\000\002\000\003\000\001\000\002\000\
\001\000\003\000\005\000\001\000\001\000\002\000\001\000\003\000\
\001\000\003\000\002\000\003\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\071\000\070\000\105\000\000\000\000\000\
\103\000\109\000\108\000\000\000\072\000\104\000\009\000\073\000\
\000\000\010\000\079\000\118\000\000\000\106\000\000\000\000\000\
\117\000\006\000\000\000\000\000\000\000\091\000\011\000\000\000\
\000\000\031\000\032\000\029\000\030\000\033\000\092\000\095\000\
\096\000\100\000\012\000\000\000\003\000\004\000\000\000\080\000\
\000\000\061\000\000\000\000\000\036\000\024\000\025\000\026\000\
\027\000\028\000\000\000\000\000\044\000\000\000\000\000\053\000\
\054\000\055\000\000\000\000\000\085\000\000\000\087\000\074\000\
\075\000\076\000\077\000\078\000\081\000\000\000\000\000\089\000\
\000\000\000\000\000\000\000\000\000\000\115\000\000\000\111\000\
\000\000\000\000\008\000\021\000\022\000\000\000\002\000\016\000\
\017\000\000\000\090\000\000\000\000\000\000\000\035\000\023\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\069\000\000\000\000\000\
\066\000\068\000\088\000\082\000\086\000\083\000\000\000\000\000\
\000\000\000\000\101\000\000\000\107\000\110\000\000\000\116\000\
\005\000\034\000\014\000\000\000\018\000\062\000\000\000\060\000\
\037\000\038\000\039\000\000\000\000\000\047\000\048\000\045\000\
\046\000\000\000\000\000\056\000\000\000\063\000\000\000\084\000\
\000\000\000\000\000\000\102\000\114\000\112\000\000\000\015\000\
\013\000\067\000\000\000\000\000\000\000\019\000\000\000\097\000\
\000\000\000\000\000\000\000\000\098\000\094\000\099\000"

let yydgoto = "\002\000\
\007\000\043\000\044\000\045\000\046\000\047\000\019\000\048\000\
\049\000\139\000\140\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\008\000\
\119\000\120\000\121\000\122\000\071\000\072\000\073\000\074\000\
\075\000\076\000\021\000\077\000\078\000\079\000\080\000\081\000\
\082\000\083\000\084\000\009\000\010\000\011\000\088\000\089\000\
\090\000\025\000\012\000"

let yysindex = "\011\000\
\001\000\000\000\243\254\000\000\000\000\000\000\237\254\008\255\
\000\000\000\000\000\000\015\255\000\000\000\000\000\000\000\000\
\042\255\000\000\000\000\000\000\007\255\000\000\052\255\036\255\
\000\000\000\000\068\255\087\255\087\255\000\000\000\000\008\000\
\087\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\070\255\000\000\000\000\014\255\000\000\
\078\255\000\000\073\255\008\000\000\000\000\000\000\000\000\000\
\000\000\000\000\050\255\038\255\000\000\019\255\133\255\000\000\
\000\000\000\000\107\255\134\255\000\000\008\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\083\255\007\255\000\000\
\127\255\129\255\130\255\201\255\145\255\000\000\008\255\000\000\
\136\255\146\255\000\000\000\000\000\000\245\254\000\000\000\000\
\000\000\246\255\000\000\008\000\008\000\008\000\000\000\000\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\000\000\154\255\157\255\
\000\000\000\000\000\000\000\000\000\000\000\000\083\255\008\000\
\008\000\035\000\000\000\093\255\000\000\000\000\048\255\000\000\
\000\000\000\000\000\000\255\254\000\000\000\000\242\254\000\000\
\000\000\000\000\000\000\050\255\050\255\000\000\000\000\000\000\
\000\000\019\255\019\255\000\000\107\255\000\000\087\255\000\000\
\030\255\085\255\035\000\000\000\000\000\000\000\008\000\000\000\
\000\000\000\000\188\255\188\255\246\255\000\000\114\255\000\000\
\085\255\188\255\188\255\188\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\194\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\070\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\096\000\000\000\000\000\000\000\000\000\
\122\000\000\000\148\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\174\000\252\000\000\000\020\001\137\255\000\000\
\000\000\000\000\221\255\163\255\000\000\171\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\166\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\174\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\200\000\226\000\000\000\000\000\000\000\
\000\000\025\001\049\001\000\000\247\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\141\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\248\255\000\000\176\000\226\255\000\000\000\000\
\000\000\121\255\000\000\182\255\024\000\000\000\208\255\000\000\
\000\000\000\000\000\000\000\000\046\000\000\000\065\000\043\000\
\000\000\000\000\000\000\090\000\093\000\000\000\128\000\238\255\
\000\000\000\000\052\000\000\000\185\255\201\000\139\255\000\000\
\000\000\000\000\000\000\195\255\133\000\000\000\000\000\000\000\
\000\000\000\000\000\000\211\000\000\000\000\000\085\000\000\000\
\000\000\000\000\000\000"

let yytablesize = 597
let yytable = "\017\000\
\006\000\094\000\070\000\104\000\168\000\087\000\123\000\015\000\
\015\000\026\000\027\000\001\000\163\000\028\000\029\000\100\000\
\124\000\126\000\100\000\092\000\093\000\169\000\137\000\141\000\
\095\000\142\000\172\000\144\000\167\000\110\000\111\000\013\000\
\138\000\030\000\018\000\031\000\003\000\178\000\004\000\032\000\
\099\000\180\000\018\000\100\000\033\000\173\000\005\000\034\000\
\035\000\036\000\037\000\038\000\016\000\132\000\085\000\123\000\
\145\000\146\000\147\000\100\000\070\000\118\000\003\000\171\000\
\004\000\160\000\112\000\113\000\022\000\086\000\143\000\027\000\
\005\000\023\000\024\000\103\000\096\000\097\000\134\000\039\000\
\108\000\109\000\040\000\015\000\041\000\026\000\027\000\015\000\
\042\000\028\000\029\000\003\000\174\000\004\000\105\000\106\000\
\107\000\161\000\162\000\175\000\176\000\005\000\098\000\003\000\
\102\000\004\000\181\000\182\000\183\000\030\000\018\000\031\000\
\101\000\005\000\100\000\032\000\087\000\165\000\138\000\164\000\
\033\000\116\000\100\000\034\000\035\000\036\000\037\000\038\000\
\103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
\103\000\103\000\103\000\103\000\103\000\093\000\177\000\093\000\
\093\000\114\000\115\000\093\000\093\000\117\000\118\000\052\000\
\052\000\148\000\149\000\039\000\154\000\155\000\040\000\128\000\
\041\000\129\000\130\000\052\000\042\000\135\000\052\000\093\000\
\093\000\093\000\052\000\133\000\052\000\093\000\150\000\151\000\
\152\000\153\000\093\000\136\000\158\000\093\000\093\000\093\000\
\093\000\093\000\159\000\179\000\015\000\059\000\026\000\027\000\
\059\000\119\000\028\000\029\000\059\000\064\000\059\000\113\000\
\065\000\015\000\091\000\026\000\027\000\156\000\125\000\028\000\
\029\000\157\000\170\000\127\000\020\000\093\000\030\000\018\000\
\093\000\014\000\093\000\166\000\032\000\000\000\093\000\000\000\
\000\000\033\000\000\000\131\000\034\000\035\000\036\000\037\000\
\038\000\032\000\000\000\000\000\057\000\000\000\033\000\000\000\
\000\000\034\000\035\000\036\000\037\000\038\000\015\000\057\000\
\026\000\027\000\057\000\000\000\028\000\029\000\057\000\000\000\
\057\000\000\000\000\000\000\000\039\000\000\000\058\000\040\000\
\015\000\041\000\026\000\027\000\000\000\042\000\028\000\029\000\
\000\000\058\000\000\000\000\000\058\000\000\000\032\000\138\000\
\058\000\000\000\058\000\033\000\000\000\000\000\034\000\035\000\
\036\000\037\000\038\000\015\000\000\000\026\000\027\000\000\000\
\032\000\028\000\029\000\000\000\000\000\033\000\000\000\000\000\
\034\000\035\000\036\000\037\000\038\000\000\000\000\000\000\000\
\003\000\000\000\004\000\000\000\000\000\030\000\000\000\000\000\
\000\000\000\000\005\000\032\000\000\000\000\000\000\000\000\000\
\033\000\000\000\000\000\034\000\035\000\036\000\037\000\038\000\
\007\000\007\000\007\000\007\000\007\000\007\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\007\000\000\000\000\000\007\000\000\000\007\000\000\000\007\000\
\007\000\007\000\001\000\001\000\001\000\001\000\001\000\001\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\000\000\
\000\000\000\000\001\000\000\000\000\000\001\000\000\000\001\000\
\000\000\001\000\001\000\001\000\020\000\020\000\020\000\020\000\
\020\000\020\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\000\000\000\000\000\000\020\000\000\000\000\000\020\000\
\000\000\020\000\000\000\020\000\000\000\020\000\035\000\035\000\
\035\000\035\000\035\000\035\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\000\000\000\000\000\000\035\000\000\000\
\000\000\035\000\000\000\000\000\000\000\035\000\000\000\035\000\
\040\000\040\000\040\000\040\000\040\000\040\000\035\000\035\000\
\035\000\035\000\035\000\035\000\035\000\000\000\000\000\000\000\
\040\000\000\000\000\000\040\000\000\000\000\000\000\000\040\000\
\000\000\040\000\041\000\041\000\041\000\041\000\041\000\041\000\
\040\000\040\000\000\000\000\000\000\000\040\000\040\000\000\000\
\000\000\000\000\041\000\000\000\000\000\041\000\000\000\000\000\
\000\000\041\000\000\000\041\000\042\000\042\000\042\000\042\000\
\042\000\042\000\041\000\041\000\000\000\000\000\000\000\041\000\
\041\000\000\000\000\000\000\000\042\000\000\000\000\000\042\000\
\000\000\000\000\000\000\042\000\000\000\042\000\043\000\043\000\
\043\000\043\000\043\000\043\000\042\000\042\000\000\000\000\000\
\000\000\042\000\042\000\000\000\000\000\000\000\043\000\000\000\
\000\000\043\000\000\000\000\000\000\000\043\000\000\000\043\000\
\049\000\049\000\049\000\049\000\000\000\050\000\050\000\050\000\
\050\000\000\000\000\000\043\000\043\000\000\000\049\000\000\000\
\000\000\049\000\000\000\050\000\000\000\049\000\050\000\049\000\
\000\000\000\000\050\000\000\000\050\000\051\000\051\000\051\000\
\051\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\051\000\000\000\000\000\051\000\000\000\
\000\000\000\000\051\000\000\000\051\000"

let yycheck = "\008\000\
\000\000\032\000\021\000\052\000\140\000\024\000\078\000\001\001\
\001\001\003\001\004\001\001\000\130\000\007\001\008\001\030\001\
\078\000\079\000\030\001\028\000\029\000\036\001\034\001\098\000\
\033\000\100\000\162\000\102\000\030\001\011\001\012\001\045\001\
\034\001\027\001\028\001\029\001\056\001\173\000\058\001\033\001\
\027\001\177\000\028\001\030\001\038\001\163\000\066\001\041\001\
\042\001\043\001\044\001\045\001\045\001\084\000\003\001\127\000\
\105\000\106\000\107\000\030\001\079\000\070\000\056\001\034\001\
\058\001\127\000\048\001\049\001\027\001\034\001\101\000\004\001\
\066\001\032\001\033\001\052\000\007\001\008\001\087\000\073\001\
\043\001\044\001\076\001\001\001\078\001\003\001\004\001\001\001\
\082\001\007\001\008\001\056\001\167\000\058\001\045\001\046\001\
\047\001\128\000\129\000\171\000\172\000\066\001\033\001\056\001\
\032\001\058\001\178\000\179\000\180\000\027\001\028\001\029\001\
\035\001\066\001\030\001\033\001\135\000\070\001\034\001\027\001\
\038\001\015\001\030\001\041\001\042\001\043\001\044\001\045\001\
\105\000\106\000\107\000\108\000\109\000\110\000\111\000\112\000\
\113\000\114\000\115\000\116\000\117\000\001\001\173\000\003\001\
\004\001\013\001\014\001\007\001\008\001\016\001\159\000\015\001\
\016\001\108\000\109\000\073\001\114\000\115\000\076\001\033\001\
\078\001\033\001\033\001\027\001\082\001\030\001\030\001\027\001\
\028\001\029\001\034\001\027\001\036\001\033\001\110\000\111\000\
\112\000\113\000\038\001\034\001\027\001\041\001\042\001\043\001\
\044\001\045\001\030\001\074\001\001\001\027\001\003\001\004\001\
\030\001\000\000\007\001\008\001\034\001\027\001\036\001\034\001\
\027\001\001\001\027\000\003\001\004\001\116\000\079\000\007\001\
\008\001\117\000\159\000\079\000\012\000\073\001\027\001\028\001\
\076\001\007\000\078\001\135\000\033\001\255\255\082\001\255\255\
\255\255\038\001\255\255\027\001\041\001\042\001\043\001\044\001\
\045\001\033\001\255\255\255\255\016\001\255\255\038\001\255\255\
\255\255\041\001\042\001\043\001\044\001\045\001\001\001\027\001\
\003\001\004\001\030\001\255\255\007\001\008\001\034\001\255\255\
\036\001\255\255\255\255\255\255\073\001\255\255\016\001\076\001\
\001\001\078\001\003\001\004\001\255\255\082\001\007\001\008\001\
\255\255\027\001\255\255\255\255\030\001\255\255\033\001\034\001\
\034\001\255\255\036\001\038\001\255\255\255\255\041\001\042\001\
\043\001\044\001\045\001\001\001\255\255\003\001\004\001\255\255\
\033\001\007\001\008\001\255\255\255\255\038\001\255\255\255\255\
\041\001\042\001\043\001\044\001\045\001\255\255\255\255\255\255\
\056\001\255\255\058\001\255\255\255\255\027\001\255\255\255\255\
\255\255\255\255\066\001\033\001\255\255\255\255\255\255\255\255\
\038\001\255\255\255\255\041\001\042\001\043\001\044\001\045\001\
\011\001\012\001\013\001\014\001\015\001\016\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\027\001\255\255\255\255\030\001\255\255\032\001\255\255\034\001\
\035\001\036\001\011\001\012\001\013\001\014\001\015\001\016\001\
\043\001\044\001\045\001\046\001\047\001\048\001\049\001\255\255\
\255\255\255\255\027\001\255\255\255\255\030\001\255\255\032\001\
\255\255\034\001\035\001\036\001\011\001\012\001\013\001\014\001\
\015\001\016\001\043\001\044\001\045\001\046\001\047\001\048\001\
\049\001\255\255\255\255\255\255\027\001\255\255\255\255\030\001\
\255\255\032\001\255\255\034\001\255\255\036\001\011\001\012\001\
\013\001\014\001\015\001\016\001\043\001\044\001\045\001\046\001\
\047\001\048\001\049\001\255\255\255\255\255\255\027\001\255\255\
\255\255\030\001\255\255\255\255\255\255\034\001\255\255\036\001\
\011\001\012\001\013\001\014\001\015\001\016\001\043\001\044\001\
\045\001\046\001\047\001\048\001\049\001\255\255\255\255\255\255\
\027\001\255\255\255\255\030\001\255\255\255\255\255\255\034\001\
\255\255\036\001\011\001\012\001\013\001\014\001\015\001\016\001\
\043\001\044\001\255\255\255\255\255\255\048\001\049\001\255\255\
\255\255\255\255\027\001\255\255\255\255\030\001\255\255\255\255\
\255\255\034\001\255\255\036\001\011\001\012\001\013\001\014\001\
\015\001\016\001\043\001\044\001\255\255\255\255\255\255\048\001\
\049\001\255\255\255\255\255\255\027\001\255\255\255\255\030\001\
\255\255\255\255\255\255\034\001\255\255\036\001\011\001\012\001\
\013\001\014\001\015\001\016\001\043\001\044\001\255\255\255\255\
\255\255\048\001\049\001\255\255\255\255\255\255\027\001\255\255\
\255\255\030\001\255\255\255\255\255\255\034\001\255\255\036\001\
\013\001\014\001\015\001\016\001\255\255\013\001\014\001\015\001\
\016\001\255\255\255\255\048\001\049\001\255\255\027\001\255\255\
\255\255\030\001\255\255\027\001\255\255\034\001\030\001\036\001\
\255\255\255\255\034\001\255\255\036\001\013\001\014\001\015\001\
\016\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\027\001\255\255\255\255\030\001\255\255\
\255\255\255\255\034\001\255\255\036\001"

let yynames_const = "\
  SIZEOF\000\
  PTR_OP\000\
  INC_OP\000\
  DEC_OP\000\
  LEFT_OP\000\
  RIGHT_OP\000\
  LE_OP\000\
  GE_OP\000\
  EQ_OP\000\
  NE_OP\000\
  AND_OP\000\
  OR_OP\000\
  MUL_ASSIGN\000\
  DIV_ASSIGN\000\
  MOD_ASSIGN\000\
  ADD_ASSIGN\000\
  SUB_ASSIGN\000\
  LEFT_ASSIGN\000\
  RIGHT_ASSIGN\000\
  AND_ASSIGN\000\
  XOR_ASSIGN\000\
  OR_ASSIGN\000\
  SEMI_CHR\000\
  OPEN_BRACE_CHR\000\
  CLOSE_BRACE_CHR\000\
  COMMA_CHR\000\
  COLON_CHR\000\
  EQ_CHR\000\
  OPEN_PAREN_CHR\000\
  CLOSE_PAREN_CHR\000\
  OPEN_BRACKET_CHR\000\
  CLOSE_BRACKET_CHR\000\
  DOT_CHR\000\
  AND_CHR\000\
  OR_CHR\000\
  XOR_CHR\000\
  BANG_CHR\000\
  TILDE_CHR\000\
  ADD_CHR\000\
  SUB_CHR\000\
  STAR_CHR\000\
  DIV_CHR\000\
  MOD_CHR\000\
  OPEN_ANGLE_CHR\000\
  CLOSE_ANGLE_CHR\000\
  QUES_CHR\000\
  TYPEDEF\000\
  EXTERN\000\
  STATIC\000\
  AUTO\000\
  REGISTER\000\
  CHAR\000\
  SHORT\000\
  INTEGER\000\
  LONG\000\
  SIGNED\000\
  UNSIGNED\000\
  FLOATING\000\
  DOUBLE\000\
  CONST\000\
  VOLATILE\000\
  VOID\000\
  STRUCT\000\
  UNION\000\
  ENUM\000\
  ELLIPSIS\000\
  EOF\000\
  CASE\000\
  DEFAULT\000\
  IF\000\
  ELSE\000\
  SWITCH\000\
  WHILE\000\
  DO\000\
  FOR\000\
  GOTO\000\
  CONTINUE\000\
  BREAK\000\
  RETURN\000\
  ASM\000\
  "

let yynames_block = "\
  IDENTIFIER\000\
  TYPE_NAME\000\
  CONSTANT\000\
  STRING_LITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'identifier) in
    Obj.repr(
# 71 "CLessGram.mly"
             ( Var _1 )
# 603 "CLessGram.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'identifier) in
    Obj.repr(
# 72 "CLessGram.mly"
                         ( get_reference _2)
# 610 "CLessGram.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 73 "CLessGram.mly"
                   ( IntegerLiteral _1 )
# 617 "CLessGram.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'string_literal) in
    Obj.repr(
# 74 "CLessGram.mly"
                         ( StringLiteral _1 )
# 624 "CLessGram.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 75 "CLessGram.mly"
                                                    ( _2 )
# 631 "CLessGram.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 78 "CLessGram.mly"
                    ( _1 )
# 638 "CLessGram.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "CLessGram.mly"
                         ( _1 )
# 645 "CLessGram.ml"
               : 'string_literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'string_literal) in
    Obj.repr(
# 83 "CLessGram.mly"
            (
              (_1) ^ (_2)
            )
# 655 "CLessGram.ml"
               : 'string_literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "CLessGram.mly"
                              (  _1 )
# 662 "CLessGram.ml"
               : 'identifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "CLessGram.mly"
                              ( () )
# 668 "CLessGram.ml"
               : 'open_brace))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "CLessGram.mly"
                              ( () )
# 674 "CLessGram.ml"
               : 'close_brace))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary_expression) in
    Obj.repr(
# 94 "CLessGram.mly"
                     ( _1 )
# 681 "CLessGram.ml"
               : 'postfix_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'postfix_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 96 "CLessGram.mly"
        ( BOp (_1,get_index (), _3) )
# 689 "CLessGram.ml"
               : 'postfix_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'close_paren) in
    Obj.repr(
# 98 "CLessGram.mly"
        (
           Call (_1, [])
        )
# 699 "CLessGram.ml"
               : 'postfix_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'argument_expression_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'close_paren) in
    Obj.repr(
# 102 "CLessGram.mly"
        (
                Call (_1, List.rev _3)
        )
# 710 "CLessGram.ml"
               : 'postfix_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'identifier) in
    Obj.repr(
# 105 "CLessGram.mly"
                      ( BOp( Set(_1,BOp(Var _1,Add,IntegerLiteral 1) ) ,Sub,IntegerLiteral 1 )  )
# 717 "CLessGram.ml"
               : 'postfix_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'identifier) in
    Obj.repr(
# 106 "CLessGram.mly"
                      ( BOp( Set(_1,BOp(Var _1,Sub,IntegerLiteral 1) ) ,Add,IntegerLiteral 1)  )
# 724 "CLessGram.ml"
               : 'postfix_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expression) in
    Obj.repr(
# 112 "CLessGram.mly"
                                ( [_1] )
# 731 "CLessGram.ml"
               : 'argument_expression_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'argument_expression_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expression) in
    Obj.repr(
# 113 "CLessGram.mly"
                                                                   (
          _3 :: _1 )
# 740 "CLessGram.ml"
               : 'argument_expression_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'postfix_expression) in
    Obj.repr(
# 118 "CLessGram.mly"
                             ( _1 )
# 747 "CLessGram.ml"
               : 'unary_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'identifier) in
    Obj.repr(
# 119 "CLessGram.mly"
                            ( Set(_2,BOp(Var _2, Add, IntegerLiteral 1)) )
# 754 "CLessGram.ml"
               : 'unary_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'identifier) in
    Obj.repr(
# 120 "CLessGram.mly"
                            ( Set(_2,BOp(Var _2, Sub, IntegerLiteral 1)) )
# 761 "CLessGram.ml"
               : 'unary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'unary_operator) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cast_expression) in
    Obj.repr(
# 122 "CLessGram.mly"
        (
          let  c = _1 in
          match c with
              ADD_CHR -> _2
            | SUB_CHR -> UOp (MinusM, _2)
            | BANG_CHR -> UOp (Not, _2)
            | STAR_CHR -> UOp (get_dereference (), _2)
            | _-> failwith "error"
            )
# 777 "CLessGram.ml"
               : 'unary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'add_chr) in
    Obj.repr(
# 134 "CLessGram.mly"
                    ( _1 )
# 784 "CLessGram.ml"
               : 'unary_operator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sub_chr) in
    Obj.repr(
# 135 "CLessGram.mly"
                    ( _1 )
# 791 "CLessGram.ml"
               : 'unary_operator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bang_chr) in
    Obj.repr(
# 136 "CLessGram.mly"
                    ( _1 )
# 798 "CLessGram.ml"
               : 'unary_operator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tilde_chr) in
    Obj.repr(
# 137 "CLessGram.mly"
                    ( _1 )
# 805 "CLessGram.ml"
               : 'unary_operator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'star_chr) in
    Obj.repr(
# 138 "CLessGram.mly"
                    ( _1 )
# 812 "CLessGram.ml"
               : 'unary_operator))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "CLessGram.mly"
                        ( ADD_CHR   )
# 818 "CLessGram.ml"
               : 'add_chr))
; (fun __caml_parser_env ->
    Obj.repr(
# 142 "CLessGram.mly"
                        ( SUB_CHR   )
# 824 "CLessGram.ml"
               : 'sub_chr))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "CLessGram.mly"
                        ( BANG_CHR  )
# 830 "CLessGram.ml"
               : 'bang_chr))
; (fun __caml_parser_env ->
    Obj.repr(
# 144 "CLessGram.mly"
                        ( TILDE_CHR )
# 836 "CLessGram.ml"
               : 'tilde_chr))
; (fun __caml_parser_env ->
    Obj.repr(
# 145 "CLessGram.mly"
                        ( STAR_CHR  )
# 842 "CLessGram.ml"
               : 'star_chr))
; (fun __caml_parser_env ->
    Obj.repr(
# 147 "CLessGram.mly"
                              ( () )
# 848 "CLessGram.ml"
               : 'close_paren))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expression) in
    Obj.repr(
# 150 "CLessGram.mly"
                           ( _1 )
# 855 "CLessGram.ml"
               : 'cast_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cast_expression) in
    Obj.repr(
# 153 "CLessGram.mly"
                          ( _1 )
# 862 "CLessGram.ml"
               : 'multiplicative_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'multiplicative_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cast_expression) in
    Obj.repr(
# 155 "CLessGram.mly"
        (
          BOp (_1, Mult , _3)
        )
# 872 "CLessGram.ml"
               : 'multiplicative_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'multiplicative_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cast_expression) in
    Obj.repr(
# 159 "CLessGram.mly"
        (
                BOp (_1, Div , _3)
        )
# 882 "CLessGram.ml"
               : 'multiplicative_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'multiplicative_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cast_expression) in
    Obj.repr(
# 163 "CLessGram.mly"
        (
                BOp (_1, Mod , _3)
        )
# 892 "CLessGram.ml"
               : 'multiplicative_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'multiplicative_expression) in
    Obj.repr(
# 170 "CLessGram.mly"
            ( _1 )
# 899 "CLessGram.ml"
               : 'additive_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'additive_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'multiplicative_expression) in
    Obj.repr(
# 172 "CLessGram.mly"
        (
          BOp (_1, Add , _3)
        )
# 909 "CLessGram.ml"
               : 'additive_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'additive_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'multiplicative_expression) in
    Obj.repr(
# 176 "CLessGram.mly"
        (
          BOp (_1, Sub , _3)
        )
# 919 "CLessGram.ml"
               : 'additive_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'additive_expression) in
    Obj.repr(
# 182 "CLessGram.mly"
                              ( _1 )
# 926 "CLessGram.ml"
               : 'shift_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'shift_expression) in
    Obj.repr(
# 186 "CLessGram.mly"
                           ( _1 )
# 933 "CLessGram.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'shift_expression) in
    Obj.repr(
# 188 "CLessGram.mly"
        (
          BOp( _1, LL , _3)
        )
# 943 "CLessGram.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'shift_expression) in
    Obj.repr(
# 192 "CLessGram.mly"
        (
          BOp( _3, LL , _1)
        )
# 953 "CLessGram.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'shift_expression) in
    Obj.repr(
# 196 "CLessGram.mly"
        (
          BOp( _1, LE , _3)
        )
# 963 "CLessGram.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'shift_expression) in
    Obj.repr(
# 200 "CLessGram.mly"
        (
          BOp( _3, LE , _1)
        )
# 973 "CLessGram.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'relational_expression) in
    Obj.repr(
# 206 "CLessGram.mly"
                                ( _1 )
# 980 "CLessGram.ml"
               : 'equality_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'equality_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'relational_expression) in
    Obj.repr(
# 208 "CLessGram.mly"
        (
          BOp( _1, EQ , _3)
        )
# 990 "CLessGram.ml"
               : 'equality_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'equality_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'relational_expression) in
    Obj.repr(
# 212 "CLessGram.mly"
        (
          BOp( _1, NEQ , _3)
        )
# 1000 "CLessGram.ml"
               : 'equality_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'equality_expression) in
    Obj.repr(
# 218 "CLessGram.mly"
                              ( _1 )
# 1007 "CLessGram.ml"
               : 'and_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_expression) in
    Obj.repr(
# 222 "CLessGram.mly"
                         ( _1 )
# 1014 "CLessGram.ml"
               : 'exclusive_or_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exclusive_or_expression) in
    Obj.repr(
# 226 "CLessGram.mly"
                                  ( _1 )
# 1021 "CLessGram.ml"
               : 'inclusive_or_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'inclusive_or_expression) in
    Obj.repr(
# 230 "CLessGram.mly"
                                  ( _1 )
# 1028 "CLessGram.ml"
               : 'logical_and_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'logical_and_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'inclusive_or_expression) in
    Obj.repr(
# 232 "CLessGram.mly"
        (
                BOp (_1, And , _3)
        )
# 1038 "CLessGram.ml"
               : 'logical_and_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logical_and_expression) in
    Obj.repr(
# 238 "CLessGram.mly"
                                 ( _1 )
# 1045 "CLessGram.ml"
               : 'logical_or_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'logical_or_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'logical_and_expression) in
    Obj.repr(
# 240 "CLessGram.mly"
        (
                BOp (_1, Or , _3)
        )
# 1055 "CLessGram.ml"
               : 'logical_or_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logical_or_expression) in
    Obj.repr(
# 246 "CLessGram.mly"
                              ( _1 )
# 1062 "CLessGram.ml"
               : 'assignment_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unary_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expression) in
    Obj.repr(
# 249 "CLessGram.mly"
            (
             let left = _1 in
             match left with
               Var x -> Set (x, _3)
             | UOp (uop,e) when (try uop = get_dereference () with _-> false)
               -> BOp (e,get_setReference (),_3)
             | BOp (Var x, bop , i) when (try bop = get_index () with _-> false)
               -> get_setArray (x, i, _3)
             |_ -> failwith "error"
           )
# 1079 "CLessGram.ml"
               : 'assignment_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expression) in
    Obj.repr(
# 262 "CLessGram.mly"
                                ( _1 )
# 1086 "CLessGram.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignment_expression) in
    Obj.repr(
# 264 "CLessGram.mly"
        (
          Seq [_1; _3]
        )
# 1096 "CLessGram.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_specifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'optional_init_declarator_list) in
    Obj.repr(
# 271 "CLessGram.mly"
        ( List.rev _2 )
# 1104 "CLessGram.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    Obj.repr(
# 275 "CLessGram.mly"
          ( [] )
# 1110 "CLessGram.ml"
               : 'optional_init_declarator_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'init_declarator_list) in
    Obj.repr(
# 276 "CLessGram.mly"
                               ( _1 )
# 1117 "CLessGram.ml"
               : 'optional_init_declarator_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'init_declarator) in
    Obj.repr(
# 282 "CLessGram.mly"
            ( [_1] )
# 1124 "CLessGram.ml"
               : 'init_declarator_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'init_declarator_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'init_declarator) in
    Obj.repr(
# 284 "CLessGram.mly"
            ( _3 :: _1 )
# 1132 "CLessGram.ml"
               : 'init_declarator_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'declarator) in
    Obj.repr(
# 287 "CLessGram.mly"
                            ( _1 )
# 1139 "CLessGram.ml"
               : 'init_declarator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'identifier) in
    Obj.repr(
# 290 "CLessGram.mly"
                     ( let x = _1 in x )
# 1146 "CLessGram.ml"
               : 'declarator))
; (fun __caml_parser_env ->
    Obj.repr(
# 294 "CLessGram.mly"
               (())
# 1152 "CLessGram.ml"
               : 'type_specifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 295 "CLessGram.mly"
                  ( () )
# 1158 "CLessGram.ml"
               : 'type_specifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 296 "CLessGram.mly"
                        ( () )
# 1164 "CLessGram.ml"
               : 'type_specifier))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_specifier) in
    Obj.repr(
# 297 "CLessGram.mly"
                                  ( () )
# 1171 "CLessGram.ml"
               : 'type_specifier))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_statement) in
    Obj.repr(
# 300 "CLessGram.mly"
            ( _1 )
# 1178 "CLessGram.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression_statement) in
    Obj.repr(
# 302 "CLessGram.mly"
            ( Expr _1 )
# 1185 "CLessGram.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'selection_statement) in
    Obj.repr(
# 304 "CLessGram.mly"
            ( _1 )
# 1192 "CLessGram.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'iteration_statement) in
    Obj.repr(
# 306 "CLessGram.mly"
            ( _1 )
# 1199 "CLessGram.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'jump_statement) in
    Obj.repr(
# 308 "CLessGram.mly"
            ( _1 )
# 1206 "CLessGram.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'open_brace) in
    Obj.repr(
# 311 "CLessGram.mly"
                        ( _1 )
# 1213 "CLessGram.ml"
               : 'open_block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'close_brace) in
    Obj.repr(
# 312 "CLessGram.mly"
                          ( _1 )
# 1220 "CLessGram.ml"
               : 'close_block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'open_block) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'close_block) in
    Obj.repr(
# 316 "CLessGram.mly"
        ( CompoundStmt ([], []) )
# 1228 "CLessGram.ml"
               : 'compound_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'open_block) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'close_block) in
    Obj.repr(
# 318 "CLessGram.mly"
        ( CompoundStmt ([], List.rev _2) )
# 1237 "CLessGram.ml"
               : 'compound_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'open_block) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'declaration_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'close_block) in
    Obj.repr(
# 320 "CLessGram.mly"
        ( CompoundStmt (_2, []) )
# 1246 "CLessGram.ml"
               : 'compound_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'open_block) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'declaration_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'close_block) in
    Obj.repr(
# 322 "CLessGram.mly"
        ( CompoundStmt (_2, List.rev _3) )
# 1256 "CLessGram.ml"
               : 'compound_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'declaration) in
    Obj.repr(
# 328 "CLessGram.mly"
          ( _1 )
# 1263 "CLessGram.ml"
               : 'declaration_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'declaration_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'declaration) in
    Obj.repr(
# 330 "CLessGram.mly"
          ( _1 @ _2 )
# 1271 "CLessGram.ml"
               : 'declaration_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 336 "CLessGram.mly"
          ( [_1] )
# 1278 "CLessGram.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 338 "CLessGram.mly"
          ( _2 :: _1 )
# 1286 "CLessGram.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'semi_chr) in
    Obj.repr(
# 343 "CLessGram.mly"
            ( Seq [] )
# 1293 "CLessGram.ml"
               : 'expression_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 345 "CLessGram.mly"
            ( _1 )
# 1300 "CLessGram.ml"
               : 'expression_statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 348 "CLessGram.mly"
                    ( () )
# 1306 "CLessGram.ml"
               : 'semi_chr))
; (fun __caml_parser_env ->
    Obj.repr(
# 350 "CLessGram.mly"
          ( () )
# 1312 "CLessGram.ml"
               : 'ifkw))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'ifkw) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 354 "CLessGram.mly"
        (
          IfStmt (_3, _5, (CompoundStmt ([], [])))
        )
# 1323 "CLessGram.ml"
               : 'selection_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'ifkw) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 358 "CLessGram.mly"
        (
          IfStmt (_3, _5, _7)
        )
# 1335 "CLessGram.ml"
               : 'selection_statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 363 "CLessGram.mly"
                ( () )
# 1341 "CLessGram.ml"
               : 'whilekw))
; (fun __caml_parser_env ->
    Obj.repr(
# 364 "CLessGram.mly"
            ( () )
# 1347 "CLessGram.ml"
               : 'forkw))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'whilekw) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'close_paren) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 367 "CLessGram.mly"
           (
            WhileStmt (_3, _5)
           )
# 1359 "CLessGram.ml"
               : 'iteration_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'forkw) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expression_statement) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expression_statement) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'close_paren) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 372 "CLessGram.mly"
        (
         CompoundStmt ([], [ Expr _3 ;
                                WhileStmt (_4, _6)])
        )
# 1373 "CLessGram.ml"
               : 'iteration_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'forkw) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expression_statement) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'expression_statement) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'close_paren) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 378 "CLessGram.mly"
        (
          CompoundStmt ([], [ Expr _3;
                                 WhileStmt (_4,
                                  CompoundStmt ([], [_7; Expr _5]))])
        )
# 1389 "CLessGram.ml"
               : 'iteration_statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 385 "CLessGram.mly"
                ( )
# 1395 "CLessGram.ml"
               : 'return))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'return) in
    Obj.repr(
# 389 "CLessGram.mly"
            ( ReturnStmt None )
# 1402 "CLessGram.ml"
               : 'jump_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'return) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 391 "CLessGram.mly"
            (  ReturnStmt (Some _2) )
# 1410 "CLessGram.ml"
               : 'jump_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'external_declaration) in
    Obj.repr(
# 396 "CLessGram.mly"
          ( _1 )
# 1417 "CLessGram.ml"
               : (CLessType.declaration list)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : (CLessType.declaration list)) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'external_declaration) in
    Obj.repr(
# 398 "CLessGram.mly"
          ( _1 @ _2 )
# 1425 "CLessGram.ml"
               : (CLessType.declaration list)))
; (fun __caml_parser_env ->
    Obj.repr(
# 400 "CLessGram.mly"
          ( [] )
# 1431 "CLessGram.ml"
               : (CLessType.declaration list)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_specifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'identifier) in
    Obj.repr(
# 404 "CLessGram.mly"
                                            ( VarDecl(_2,None) )
# 1439 "CLessGram.ml"
               : 'glob_var_declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'type_specifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'identifier) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 405 "CLessGram.mly"
                                                            ( VarDecl(_2,Some _4) )
# 1448 "CLessGram.ml"
               : 'glob_var_declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'function_definition) in
    Obj.repr(
# 409 "CLessGram.mly"
            ( [_1] )
# 1455 "CLessGram.ml"
               : 'external_declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'glob_var_declaration) in
    Obj.repr(
# 411 "CLessGram.mly"
            ( [_1] )
# 1462 "CLessGram.ml"
               : 'external_declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_specifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'identifier) in
    Obj.repr(
# 414 "CLessGram.mly"
                                                 ( _2 )
# 1470 "CLessGram.ml"
               : 'parameter_declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'parameter_declaration) in
    Obj.repr(
# 419 "CLessGram.mly"
          ( [_1] )
# 1477 "CLessGram.ml"
               : 'parameter_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parameter_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parameter_declaration) in
    Obj.repr(
# 421 "CLessGram.mly"
          ( _3 :: _1 )
# 1485 "CLessGram.ml"
               : 'parameter_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'parameter_list) in
    Obj.repr(
# 425 "CLessGram.mly"
                         ( List.rev _1)
# 1492 "CLessGram.ml"
               : 'parameter_type_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parameter_list) in
    Obj.repr(
# 426 "CLessGram.mly"
                                            ( List.rev _1 )
# 1499 "CLessGram.ml"
               : 'parameter_type_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 430 "CLessGram.mly"
                                         ( [] )
# 1505 "CLessGram.ml"
               : 'parameter_declarator))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'parameter_type_list) in
    Obj.repr(
# 431 "CLessGram.mly"
                                                             ( _2 )
# 1512 "CLessGram.ml"
               : 'parameter_declarator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_specifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'identifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parameter_declarator) in
    Obj.repr(
# 435 "CLessGram.mly"
        ( _2, _3 )
# 1521 "CLessGram.ml"
               : 'function_declarator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'function_declarator) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'compound_statement) in
    Obj.repr(
# 440 "CLessGram.mly"
        (
          let var, decls = _1 in
          FunctionDecl ( var, decls, _2)
        )
# 1532 "CLessGram.ml"
               : 'function_definition))
(* Entry translation_unit *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let translation_unit (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (CLessType.declaration list))
;;
