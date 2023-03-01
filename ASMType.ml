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

type arg =
  | HollArg
  | C of intLit  (** Une constante *)
  | L of string  (** Un label *)
  | HollLabel
  | R of reg  (** Un registre *)
  | AC of intLit  (** addresse pointé par une constante *)
  | ARI of intLit * reg
      (** adresse pointée par un registre décalé par une constante*)
  | ARL of string * reg
      (** adresse pointée par un registre décalé par un label *)

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

type asm_program = instruction list

let empty = []
let to_inst_list x = x
let add_to_prog i l = i :: l

open Printf

let print_reg o r =
  let p = output_string o in
  match r with
  | RAX -> p "%rax"
  | RBX -> p "%rbx"
  | RCX -> p "%rcx"
  | RDX -> p "%rdx"
  | RSP -> p "%rsp"
  | RBP -> p "%rbp"
  | RDI -> p "%rdi"
  | RSI -> p "%rsi"
  | R8 -> p "%r8"
  | R9 -> p "%r9"
  | R10 -> p "%r10"
  | R11 -> p "%r11"
  | R12 -> p "%r12"
  | R13 -> p "%r13"
  | R14 -> p "%r14"
  | R15 -> p "%r15"
  | AL -> p "%al"
  | RIP -> p "%rip"
  | HollReg -> failwith "HollReg"
  | M i -> p ("%m" ^ string_of_int i)

let dnpz o i = if i <> 0 then fprintf o "%i" i

let print_arg o = function
  | C (IL i) -> fprintf o "$%i" i
  | L s -> fprintf o "%s" s
  | R r -> print_reg o r
  | AC (IL i) -> fprintf o "(%i)" i
  | ARI (IL i, r) -> fprintf o "%a(%a)" dnpz i print_reg r
  | ARL (l, r) -> fprintf o "%s(%a)" l print_reg r
  | HollLabel -> failwith "HollLabel"
  | _ -> failwith "HollInt"

let print_instruction o ins =
  let p1 s a = fprintf o "\t%s\t%a\n" s print_arg a in
  let p2 s a1 a2 = fprintf o "\t%s\t%a, %a\n" s print_arg a1 print_arg a2 in
  match ins with
  | NOP -> fprintf o "\tnop\n"
  | Label a -> fprintf o "%a:\n" print_arg a
  | StrLit s ->
      fprintf o "\t.asciz \"";
      String.iter
        (function '\n' -> fprintf o "%c%c" '\\' 'n' | x -> fprintf o "%c" x)
        s;
      fprintf o "\"\n"
  | GlobVar (s, None) -> fprintf o "\t.comm\t%s,8,8\n" s
  | GlobVar (s, Some i) -> fprintf o "%s:\n\t.quad %i\n" s i
  | LinkInstr s -> fprintf o "\t.%s\n" s
  | MOV (a1, a2) -> p2 "movq" a1 a2
  | LEAQ (a1, a2) -> p2 "leaq" a1 a2
  | PUSH a -> p1 "pushq" a
  | POP a -> p1 "popq" a
  | CALL a -> p1 "callq" a
  | RET -> fprintf o "\tretq\n"
  | ADD (a1, a2) -> p2 "addq" a1 a2
  | AND (a1, a2) -> p2 "andq" a1 a2
  | SUB (a1, a2) -> p2 "subq" a1 a2
  | MUL (a1, a2) -> p2 "imulq" a1 a2
  | DIV (a1, a2) ->
      fprintf o "\tcqto\n";
      p2 "idivq" a1 a2
  | XOR (a1, a2) -> p2 "xorq" a1 a2
  | OR (a1, a2) -> p2 "orq" a1 a2
  | NEG a -> p1 "negq" a
  | NOT a -> p1 "notq" a
  | CMP (a1, a2) -> p2 "cmpq" a1 a2
  | TEST (a1, a2) -> p2 "testq" a1 a2
  | SETE a -> p1 "sete" a
  | SETNE a -> p1 "setne" a
  | SETNS a -> p1 "setns" a
  | SETG a -> p1 "setg" a
  | JMP a -> p1 "jmp" a
  | JZ a -> p1 "jz" a
  | JNZ a -> p1 "jnz" a

let map_arg la = function
  | MOV (a1, a2) -> MOV (la a1, la a2)
  | PUSH a -> PUSH (la a)
  | POP a -> POP (la a)
  | CALL a -> CALL (la a)
  | ADD (a1, a2) -> ADD (la a1, la a2)
  | MUL (a1, a2) -> MUL (la a1, la a2)
  | SUB (a1, a2) -> SUB (la a1, la a2)
  | DIV (a1, a2) -> DIV (la a1, la a2)
  | XOR (a1, a2) -> XOR (la a1, la a2)
  | OR (a1, a2) -> OR (la a1, la a2)
  | AND (a1, a2) -> AND (la a1, la a2)
  | NEG a -> NEG (la a)
  | NOT a -> NOT (la a)
  | CMP (a1, a2) -> CMP (la a1, la a2)
  | TEST (a1, a2) -> TEST (la a1, la a2)
  | SETE a -> SETE (la a)
  | SETNE a -> SETNE (la a)
  | SETNS a -> SETNS (la a)
  | SETG a -> SETG (la a)
  | LEAQ (a1, a2) -> LEAQ (la a1, la a2)
  | JMP a -> JMP (la a)
  | JZ a -> JZ (la a)
  | JNZ a -> JNZ (la a)
  | x -> x

let replace_holl_arg r i l = function
  | C HollInt -> C (IL i)
  | C HollIntM -> C (IL (-i))
  | R HollReg -> R r
  | AC HollInt -> AC (IL i)
  | AC HollIntM -> AC (IL (-i))
  | ARI (HollInt, HollReg) -> ARI (IL i, r)
  | ARI (HollIntM, HollReg) -> ARI (IL (-i), r)
  | ARI (HollInt, rh) -> ARI (IL i, rh)
  | ARI (HollIntM, rh) -> ARI (IL (-i), rh)
  | ARI (IL il, HollReg) -> ARI (IL il, r)
  | ARL (l, HollReg) -> ARL (l, r)
  | HollLabel -> L l
  | x -> x

let replace_holl t a r i l =
  map_arg (function HollArg -> a | x -> replace_holl_arg r i l x) t

let print_programme o l = List.iter (print_instruction o) (List.rev l)
