open ASMType
open CLessType
open Tools

let input = ref stdin
let output = ref stdout
let inputName = ref "stdin"
let outputName = ref "a.out"
let doLinking = ref true
let pp = ref false
let pt = ref false
let ptdot = ref false
let exec = ref None
let optim = ref 0
let interpret = ref 0
let inline = ref false
let nocall = ref false
let regassig = ref false

let _ =
  Arg.parse
    [
      ("-o", Arg.Set_string outputName, "Output file");
      ("-S", Arg.Clear doLinking, "Output assembly");
      ("-O", Arg.Set_int optim, "Optimisation level");
      ("-p", Arg.Set pp, "print preprocessing");
      ("-d", Arg.Set ptdot, "print AST as dot");
      ("-t", Arg.Set pt, "print AST");
      ("--inline", Arg.Set inline, "Use function inlining");
      ( "-i",
        Arg.Set_int interpret,
        "Interpret assembly 0 -> No; 1 -> Yes; 2 -> With debug; 3 -> Step by \
         step" );
      ("--nc", Arg.Set nocall, "");
      ("--assign-reg", Arg.Set regassig, "");
      ("--", Arg.Rest (fun s -> exec := Some s), "Execute the outputted program");
    ]
    (function
      | s ->
          inputName := s;
          input := open_in s)
    "usage"

module StringSet = Set.Make (String)

let isLinux =
  let fo = Unix.open_process_in "uname" in
  let s = input_line fo in
  close_in fo;
  String.sub s 0 5 = "Linux"

let renameExt s = if isLinux then s else "_" ^ s

let rec renameIt fs = function
  | Call (fn, el) when not (StringSet.mem fn fs) ->
      Some (Call (renameExt fn, List.map (iter_fun_expr (renameIt fs)) el))
  | _ -> None

let externalizeFun fl =
  let inFun, inVar =
    List.fold_left
      (fun (fs, vs) f ->
        match f with
        | FunctionDecl (n, _, _) -> (StringSet.add n fs, vs)
        | VarDecl (n, nv) -> (fs, (n, nv) :: vs))
      (StringSet.empty, []) fl
  in
  fl
  |> List.filter (function FunctionDecl _ -> true | _ -> false)
  |> List.map (function
       | FunctionDecl ("main", dl, s) ->
           FunctionDecl
             ( renameExt "main",
               dl,
               iter_fun_stat (renameIt inFun) (fun _ -> None) s )
       | FunctionDecl (fn, dl, s) ->
           FunctionDecl
             (fn, dl, iter_fun_stat (renameIt inFun) (fun _ -> None) s)
       | _ -> failwith "no global variable declaration")
  |> fun x -> (x, inVar)

let initProg il =
  il
  |> add_to_prog
       (if isLinux then ASMType.LinkInstr "text"
       else ASMType.LinkInstr "section __TEXT,__text,regular,pure_instructions")
  |> add_to_prog (ASMType.LinkInstr ("globl " ^ renameExt "main"))

let rosection =
  ASMType.LinkInstr
    (if isLinux then "section\t.rodata"
    else "section    __TEXT,__cstring,cstring_literals")

let datasection =
  if isLinux then ASMType.LinkInstr "data"
  else ASMType.LinkInstr "section __DATA,__data"

let _ =
  let defList =
    try
      let lexbuf = Lexing.from_channel !input in
      CLessGram.translation_unit CLessLex.ctoken lexbuf
    with Parsing.Parse_error ->
      Printf.fprintf stderr "%s:%i:%i: Parse Error\n" !inputName !cline !ccol;
      exit 1
  in

  if !pp then
    List.iter
      (fun x -> Printf.fprintf stdout "%a\n" PrintC.print_declaration x)
      defList;
  if !pt then PrintAST.print_dec_list Format.std_formatter defList;
  if !ptdot then (
    output_string stdout "digraph AST {";
    (*\nrankdir=\"LR\";\n";*)
    ignore (PrintAST.print_dot_dec_list stdout defList);
    output_string stdout "}");
  let defListExt, globVarSet = externalizeFun defList in
  let prog0 = empty in

  let varList, prog1 =
    globVarSet
    |> List.fold_left
         (fun (vl, il) (v, nv) ->
           let label = Tools.fresh_lbl ("globvar_" ^ v) in
           ( (v, label ^ "(%rip)") :: vl,
             add_to_prog (ASMType.GlobVar (label, nv)) il ))
         ([], add_to_prog datasection prog0)
  in
  let prog2 = initProg prog1 in
  if !inline then
    List.iter
      (function
        | FunctionDecl (fn, al, s) ->
            funDecList := StringMap.add fn (al, s) !funDecList
        | VarDecl _ -> ())
      defListExt;

  (*let defListExt2 = (if !optim > 1 then OptimisationAST.inlineFun defListExt
                       else defListExt ) in*)
  let gen_fun =
    (*if !regassig then GenerateReg.generate_asm_top
      else*)
    Generate.generate_asm_top
  in

  let asm_prog =
    ( ( defListExt
      |> (fun x ->
           try List.fold_left (gen_fun varList) prog2 x with
           | Code_gen_failure_expression e as exn ->
               PrintAST.perror_exp e;
               raise exn
           | Code_gen_failure_statment s as exn ->
               PrintAST.perror_stat s;
               raise exn
           | e ->
               Printf.fprintf stderr "Exception in Generate.ml\n";
               raise e)
      |> (fun x -> add_to_prog rosection x)
      |> Hashtbl.fold
           (fun a b il ->
             il
             |> add_to_prog (ASMType.Label (L b))
             |> add_to_prog (ASMType.StrLit a))
           string_tab
      |> to_inst_list |> List.rev
      |> fun y ->
        if !regassig then (
          let x = Optimisation.apply !optim Optimisation.optimise y in
          List.iter (ASMType.print_instruction stdout) x;
          let reg = Reg_assignment.compute_life x in
          Reg_assignment.print_life Format.std_formatter reg;
          let graph = Reg_assignment.compute_graph reg in
          Reg_assignment.colorize graph [ 0; 1; 2; 3 ];
          Reg_assignment.to_dot Format.std_formatter
            (fun f (i, _) -> Format.fprintf f "%%m%i" i)
            graph;
          let map_reg i =
            match graph.(i) with
            | _, _, None -> failwith "No color register"
            | _, _, Some c -> [| RAX; RBX; RCX; RDX |].(c)
          in
          List.map
            (fun inst ->
              map_arg (function R (M i) -> R (map_reg i) | x -> x) inst)
            x)
        else y )
    |> fun x -> Optimisation.apply !optim Optimisation.optimise x )
    |> fun x ->
    if !nocall then
      List.filter (function ASMType.CALL _ -> false | _ -> true) x
    else x
  in
  if !doLinking then
    let o =
      if isLinux then
        Unix.open_process_out
          ("cc -no-pie -x assembler -o " ^ !outputName ^ " -")
      else
        Unix.open_process_out
          ("cc -no-pie -Wno-unused-command-line-argument -arch x86_64 -x \
            assembler -o " ^ !outputName ^ " -")
    in
    output := o
  else if !outputName <> "a.out" then output := open_out !outputName;

  List.iter (ASMType.print_instruction !output) asm_prog;

  if !doLinking then ignore (Unix.close_process_out !output);

  if !interpret > 0 then (
    let linked, rip =
      let alloc = Interpreter.allocator (renameExt "malloc") in
      Interpreter.link (renameExt "main") (List.rev_append alloc asm_prog)
    in
    if !interpret > 1 then
      Array.iteri
        (fun i ins ->
          let ins2 = ASMType.map_arg Interpreter.unbox_arg ins in
          Printf.printf "%i>%a" i ASMType.print_instruction ins2)
        linked;
    let machine = Interpreter.new_state (linked, rip) 1024 in
    exit (Interpreter.run_machine !interpret machine))

let _ =
  match !exec with
  | Some e -> ignore (Sys.command ("./" ^ !outputName ^ " " ^ e))
  | _ -> ()
