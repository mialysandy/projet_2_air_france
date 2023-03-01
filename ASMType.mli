(** Représentation symbolique d'assembleur et fonctions d'impression *)

(** {2 Définitions des types }*)

(** Registres *)
type reg =
  | RAX
  | RBX
  | RCX
  | RDX
  | RBP
  | RSP
  | RSI
  | RDI
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | AL
  | RIP
  | HollReg
  | M of int

type intLit = HollInt | HollIntM | IL of int

(** Arguments d'une instruction assembleur *)
type arg =
  | HollArg
  | C of intLit
  | L of string
  | HollLabel
  | R of reg
  | AC of intLit
  | ARI of intLit * reg
  | ARL of string * reg

(** Instruction assembleur *)
type instruction =
  | Label of arg
  | StrLit of string
  | GlobVar of string * int option
  | LinkInstr of string
  | MOV of arg * arg
  | PUSH of arg
  | POP of arg
  | CALL of arg
  | RET
  | ADD of arg * arg
  | MUL of arg * arg
  | SUB of arg * arg
  | DIV of arg * arg
  | XOR of arg * arg
  | OR of arg * arg
  | AND of arg * arg
  | NEG of arg
  | NOT of arg
  | CMP of arg * arg
  | TEST of arg * arg
  | SETE of arg
  | SETNE of arg
  | SETNS of arg
  | SETG of arg
  | LEAQ of arg * arg
  | JMP of arg
  | JZ of arg
  | JNZ of arg
  | NOP

type asm_program
(** programme assembleur *)

val empty : asm_program

val add_to_prog : instruction -> asm_program -> asm_program
(** Ajoute une instruction à la fin d'un programme assembleur *)

val to_inst_list : asm_program -> instruction list

(** {2 Fonctions d'impression } *)

val print_reg : out_channel -> reg -> unit
(** Impresssion d'un registre*)

val dnpz : out_channel -> int -> unit
val print_arg : out_channel -> arg -> unit
val print_instruction : out_channel -> instruction -> unit
val map_arg : (arg -> arg) -> instruction -> instruction
val replace_holl_arg : reg -> int -> string -> arg -> arg
val replace_holl : instruction -> arg -> reg -> int -> string -> instruction
val print_programme : out_channel -> instruction list -> unit
