open ASMType
open Tools

let reg_to_mem = function
  | RAX -> 0
  | RBX -> 1
  | RCX -> 2
  | RDX -> 3
  | RBP -> 4
  | RSP -> 5
  | RSI -> 6
  | RDI -> 7
  | R8 -> 8
  | R9 -> 9
  | R10 -> 10
  | R11 -> 11
  | R12 -> 12
  | R13 -> 13
  | R14 -> 14
  | R15 -> 15
  | RIP -> 16
  | AL -> 0
  | HollReg -> failwith "HollReg"
  | M _ -> failwith "NoRegistre"

type state = {
  prog : instruction array;
  register : int array;
  stack : int array;
  heap : int array;
  mutable zf : bool;
  mutable sf : bool;
}

let pe () = print_char (char_of_int 0x1b)

let print_color s c =
  pe ();
  Printf.printf "[1;%im%s" c s;
  pe ();
  print_string "[0m"

let prog_bit = 1 lsl 30
let stack_bit = 1 lsl 29
let heap_bit = 1 lsl 28
let specialfun_bit = 1 lsl 27

exception Invalid_jmp
exception Invalid_stack_addr
exception Invalid_heap_addr

let clear_bits i = i land lnot (prog_bit lor stack_bit lor heap_bit)

let unbox_prog i =
  if i land prog_bit = 0 then raise Invalid_jmp else clear_bits i

let box_prog i = i lor prog_bit

let unbox_heap i =
  if i land heap_bit = 0 then raise Invalid_heap_addr else clear_bits i

let box_heap i = i lor heap_bit

let unbox_stack i =
  if i land stack_bit = 0 then raise Invalid_stack_addr else clear_bits i

let unbox_arg = function
  | C (IL i) -> C (IL (clear_bits i))
  | AC (IL i) -> AC (IL (clear_bits i))
  | x -> x

let print_val i =
  if i land prog_bit <> 0 then
    let j = clear_bits i in
    print_color (string_of_int j) 31
  else if i land stack_bit <> 0 then
    let j = clear_bits i in
    print_color (string_of_int j) 32
  else if i land heap_bit <> 0 then
    let j = clear_bits i in
    print_color (string_of_int j) 33
  else if i <> 0 then print_color (string_of_int i) 34
  else print_int i

let get_reg m r = m.register.(reg_to_mem r)
let set_reg m r v = m.register.(reg_to_mem r) <- v

let print_header () =
  print_color "Nom des registres" 47;
  print_newline ();
  print_string "Contenu des registres: code couleur: ";
  print_color "Addresse de programme" 31;
  print_string "\t";
  print_color "Addresse de pile" 32;
  print_string "\t";
  print_color "Addresse de tas" 33;
  print_string "\t";
  print_color "Valeur entière" 34;
  print_string "\tZero\n";
  print_endline "Contenue de la pile à partir de %rsp";
  print_endline "Position de %rsp et de %rbp";
  print_endline "\tProchaine instruction éxécutée"

let print_stack m =
  let i = ref (get_reg m RSP) in
  while !i < stack_bit lor Array.length m.stack do
    print_val m.stack.(clear_bits !i);
    print_string "\t";
    i := !i + 8
  done;
  print_newline ()

let print_ptr m =
  let i = ref (get_reg m RSP) in
  while !i < stack_bit lor Array.length m.stack do
    if !i = get_reg m RSP then print_string "%rsp" else print_string " ";
    if !i = get_reg m RBP then print_string "%rbp" else print_string " ";
    print_string "\t";
    i := !i + 8
  done;
  print_newline ()

let print_flag m =
  if m.zf then print_char 'Z' else print_char '_';
  if m.sf then print_char 'S' else print_char '_'

let print_register m =
  pe ();
  Printf.printf "[1;%im" 47;
  let b = false in
  print_string "%rax\t%rbx\t%rcx\t%rdx\t%rbp\t%rsp\t%rsi\t%rdi\t%r8\t%r9\t";
  if b then print_string "%r10\t%r11\t%r12\t%r13\t%r14\t%r15\t";
  print_string "%rip\tFLAGS";
  pe ();
  print_string "[0m\n";
  Array.iteri
    (fun i x ->
      if b || i < 10 || i = 16 then (
        print_val x;
        print_string "\t"))
    m.register;
  print_flag m;
  print_string "\t\n"

module StringMap = Map.Make (String)

let allocator name =
  []
  |% GlobVar ("__alloc_count", None)
  |% Label (L name)
  |% MOV (ARL ("__alloc_count", RIP), R RAX)
  |% ADD (R RDI, ARL ("__alloc_count", RIP))
  |% RET

type symbol = ProgCount of int | VarId of int

let ifFindVar v sl t f =
  try match StringMap.find v sl with VarId vn -> t vn | _ -> f
  with Not_found -> f

let ifFindLabel l sl t f =
  try match StringMap.find l sl with ProgCount ln -> t ln | _ -> f
  with Not_found -> f

let specialfun () =
  StringMap.empty
  |> StringMap.add "printf" (ProgCount (1024 lor specialfun_bit))
  |> StringMap.add "printf1" (ProgCount (1025 lor specialfun_bit))
  |> StringMap.add "putchar" (ProgCount (1026 lor specialfun_bit))

let link main il =
  let lablist, _, _ =
    List.fold_left
      (fun (ll, ic, vc) i ->
        match i with
        | Label (L l) -> (StringMap.add l (ProgCount ic) ll, ic + 1, vc)
        | GlobVar (l, None) -> (StringMap.add l (VarId vc) ll, ic + 1, vc + 1)
        | GlobVar (l, Some _) -> (StringMap.add l (VarId vc) ll, ic + 1, vc + 1)
        | _ -> (ll, ic + 1, vc))
      (specialfun (), 0, 0)
      il
  in
  let il2, _, lablist2 =
    List.fold_left
      (fun (il, ic, sm) i ->
        match i with
        | NOP | Label _ | LinkInstr _ | GlobVar _ ->
            ( il,
              ic,
              StringMap.map
                (function
                  | ProgCount x when x > ic -> ProgCount (x - 1) | x -> x)
                sm )
        | x -> (x :: il, ic + 1, sm))
      ([], 0, lablist) il
  in
  let la = function
    | L l -> ifFindLabel l lablist2 (fun li -> C (IL (box_prog li))) (L l)
    | ARL (l, RIP) when StringMap.mem l lablist2 -> (
        match StringMap.find l lablist2 with
        | VarId vi -> AC (IL (box_heap vi))
        | ProgCount ci -> AC (IL (box_prog ci)))
    | ARL (l, r) as x ->
        ifFindVar l lablist2 (fun vi -> ARI (IL (box_heap vi), r)) x
    | x -> x
  in
  let il3 = il2 |> List.rev |> List.map (map_arg la) in
  ( Array.of_list il3,
    match StringMap.find main lablist2 with
    | ProgCount x -> x
    | _ -> failwith "No main" )

let new_state (prog, start) ss =
  let reg = Array.make 17 0 in
  reg.(reg_to_mem RIP) <- start lor prog_bit;
  let stack = Array.make ss 0 in
  stack.(ss - 8) <- (1 lsl 16) lor prog_bit;
  reg.(reg_to_mem RBP) <- (1 lsl 11) lor stack_bit;
  reg.(reg_to_mem RSP) <- (ss - 8) lor stack_bit;
  let heap = Array.make 1024 0 in
  heap.(0) <- box_heap 1;
  { prog; register = reg; stack; heap; sf = false; zf = false }

exception NotLinked

let eval_arg m = function
  | C (IL i) -> i
  | R r -> get_reg m r
  | ARI (IL i, r) ->
      let rad = get_reg m r in
      if rad land stack_bit <> 0 then m.stack.(i + unbox_stack rad)
      else m.heap.(i + unbox_heap rad)
  | AC (IL i) -> m.heap.(unbox_heap i)
  | _ -> raise NotLinked

let store_arg m v = function
  | R r -> set_reg m r v
  | ARI (IL i, r) ->
      let rad = get_reg m r in
      if rad land stack_bit <> 0 then m.stack.(i + unbox_stack rad) <- v
      else m.heap.(i + unbox_heap rad) <- v
  | AC (IL i) -> m.heap.(unbox_heap i) <- v
  | _ -> raise NotLinked

let run_one_step m =
  let ins = m.prog.(unbox_prog (get_reg m RIP)) in
  set_reg m RIP (1 + get_reg m RIP);
  let rec eval = function
    | NOP | Label _ | StrLit _ | LinkInstr _ | GlobVar _ -> ()
    | MOV (a1, a2) -> store_arg m (eval_arg m a1) a2
    | ADD (a1, a2) ->
        let v2 = eval_arg m a2 in
        let v1 = eval_arg m a1 in
        store_arg m (v1 + v2) a2
    | SUB (a1, a2) ->
        let v2 = eval_arg m a2 in
        let v1 = eval_arg m a1 in
        store_arg m (v2 - v1) a2
    | MUL (a1, a2) ->
        let v2 = eval_arg m a2 in
        let v1 = eval_arg m a1 in
        store_arg m (v2 * v1) a2
    | DIV (a1, a2) ->
        let v2 = eval_arg m a2 in
        let v1 = eval_arg m a1 in
        store_arg m (v2 / v1) a2;
        store_arg m (v2 mod v1) (R RDX)
    | AND (a1, a2) ->
        let v2 = eval_arg m a2 in
        let v1 = eval_arg m a1 in
        store_arg m (v2 land v1) a2
    | XOR (a1, a2) ->
        let v2 = eval_arg m a2 in
        let v1 = eval_arg m a1 in
        let r = v2 lxor v1 in
        m.zf <- r = 0;
        m.sf <- r < 0;
        store_arg m r a2
    | OR (a1, a2) ->
        let v2 = eval_arg m a2 in
        let v1 = eval_arg m a1 in
        store_arg m (v2 lor v1) a2
    | NOT a ->
        let v = eval_arg m a in
        store_arg m (lnot v) a
    | NEG a ->
        let v = eval_arg m a in
        store_arg m (if v = 0 then 1 else 0) a
    | TEST (a1, a2) ->
        let v2 = eval_arg m a2 in
        let v1 = eval_arg m a1 in
        m.zf <- v1 land v2 = 0;
        m.sf <- v1 land v2 < 0
    | CMP (a1, a2) ->
        let v2 = eval_arg m a2 in
        let v1 = eval_arg m a1 in
        m.zf <- v1 = v2;
        m.sf <- v2 < v1
    | PUSH a ->
        let v = eval_arg m a in
        eval (SUB (C (IL 8), R RSP));
        store_arg m v (ARI (IL 0, RSP))
    | POP a ->
        let v = eval_arg m (ARI (IL 0, RSP)) in
        store_arg m v a;
        eval (ADD (C (IL 8), R RSP))
    | RET -> eval (POP (R RIP))
    | JMP a ->
        let v = eval_arg m a in
        store_arg m v (R RIP)
    | JZ a ->
        if m.zf then
          let v = eval_arg m a in
          store_arg m v (R RIP)
    | JNZ a ->
        if not m.zf then
          let v = eval_arg m a in
          store_arg m v (R RIP)
    | CALL a ->
        let v = eval_arg m a in
        eval (PUSH (R RIP));
        store_arg m v (R RIP)
    | SETG a -> if (not m.zf) && not m.sf then store_arg m 1 a
    | SETE a -> if m.zf then store_arg m 1 a
    | SETNE a -> if not m.zf then store_arg m 1 a
    | SETNS a -> if not m.sf then store_arg m 1 a
    | LEAQ (a1, a2) ->
        let v =
          match a1 with
          | ARI (IL i, r) -> i + m.register.(reg_to_mem r)
          | AC (IL i) -> i
          | _ -> raise NotLinked
        in
        store_arg m v a2
  in
  eval ins

let run_machine level m =
  if level > 1 then print_header ();
  while get_reg m RIP != (1 lsl 16) lor prog_bit do
    if level > 1 then (
      print_register m;
      print_stack m;
      print_ptr m);
    let ins_index = unbox_prog (get_reg m RIP) in
    (if specialfun_bit land ins_index = 0 then (
     let ins = map_arg unbox_arg m.prog.(ins_index) in
     if level > 1 then print_instruction stdout ins;
     run_one_step m)
    else
      let spl = unbox_prog (get_reg m RDI) in
      let s = match m.prog.(spl) with StrLit x -> x | _ -> "" in
      print_string s;
      let v = eval_arg m (ARI (IL 0, RSP)) in
      store_arg m v (R RIP);
      let v2 = eval_arg m (R RSP) in
      store_arg m (v2 + 8) (R RSP));
    if level > 2 then ignore (read_line ())
  done;
  get_reg m RAX
