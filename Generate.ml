open CLessType
open Tools
open ASMType

type environnement = (string * string) list

let rec generate_asm_expression varl sp e il =
  match e with
  | IntegerLiteral i -> il |+ "movq $" ^ string_of_int i ^ ", %rax"

  | UOp (uop,expr) ->
    begin match uop with

    | Not -> let il2 = generate_asm_expression varl sp expr il
      in il2|+ "notq %rax"
    | MinusM ->
      let il2 = generate_asm_expression varl sp expr il
      in il2|+ "negq %rax"
    | Deref -> (** déréference i.e. *x *)
      raise (Code_gen_failure_expression e)
    end
  | BOp (expr1,bop,expr2) ->
    begin match bop with
    
    | Add ->
      let il2= generate_asm_expression varl sp expr1 il
      |+ "pushq %rax" in
         generate_asm_expression varl sp expr2 il2 
         |+ "addq (%rsp), %rax"
         |+ "addq $8, %rsp"
  
    | Sub ->
      let il2= generate_asm_expression varl sp expr2 il
      |+ "pushq %rax" in
         generate_asm_expression varl sp expr1 il2 
         |+ "subq (%rsp), %rax"
         |+ "addq $8, %rsp"
    | Div ->
         let il2 = generate_asm_expression varl sp expr2 il 
         |+ "pushq %rax"in 
         generate_asm_expression varl sp expr1 il2
         |+ "movq $0, %rdx"
         |+ "idivq (%rsp), %rax"
         |+ "addq $8, %rsp"
    | Mod ->
         let il2 = generate_asm_expression varl sp expr2 il 
         |+ "pushq %rax"in 
         generate_asm_expression varl sp expr1 il2
         |+ "movq $0, %rdx"
         |+ "idivq (%rsp), %rax"
         |+  "movq $rdx, %rax"
         |+ "addq $8, %rsp" 

    | Mult ->
         let il2= generate_asm_expression varl sp expr1 il
      |+ "pushq %rax" in
         generate_asm_expression varl sp expr2 il2 
         |+ "imulq (%rsp), %rax"
         |+ "addq $8, %rsp"
      |EQ ->
          let il2= generate_asm_expression varl sp expr1 il
          |+ "pushq %rax" in
            generate_asm_expression varl sp expr2 il2 
            |+ "cmpq (%rsp), %rax"
            |+ "sete %al"
            |+ "addq $8, %rsp"
      |NEQ ->
          let il2= generate_asm_expression varl sp expr1 il
          |+ "pushq %rax" in
            generate_asm_expression varl sp expr2 il2 
            |+ "cmpq (%rsp), %rax"
            |+ "setne %al"
            |+ "addq $8, %rsp"    
         
      |LE ->
          let il2= generate_asm_expression varl sp expr1 il
          |+ "pushq %rax" in
            generate_asm_expression varl sp expr2 il2 
            |+ "cmpq (%rsp), %rax"
            |+ "setns %al"
            |+ "addq $8, %rsp"  
            
      |LL ->
          let il2= generate_asm_expression varl sp expr1 il
          |+ "pushq %rax" in
            generate_asm_expression varl sp expr2 il2 
            |+ "cmpq (%rsp), %rax"
            |+ "setg %al"
            |+ "addq $8, %rsp"  
      |Or-> let il2= generate_asm_expression varl sp expr1 il
        |+ "pushq %rax" in
        generate_asm_expression varl sp expr2 il2 
        |+ "orq (%rsp), %rax"
        |+ "addq $8, %rsp"  
      |And ->let il2= generate_asm_expression varl sp expr1 il
      |+ "pushq %rax" in
      generate_asm_expression varl sp expr2 il2 
      |+ "andq (%rsp), %rax"
      |+ "addq $8, %rsp"            
    end
    (*pattern non exhaustif il ne gere pas le div *)
  | Var( var)-> il|+ "movq " ^ List.assoc var varl ^ ", %rax"
  | Set ( var ,expresion)-> generate_asm_expression varl sp expresion il
  |+ "movq  %rax, " ^ List.assoc var varl 
  | Call(f,[]) -> il
  |+"callq "^f
   

  
  
  | Seq _ 
  | SetArray _
  | StringLiteral _ | Ref _ ->
      raise (Code_gen_failure_expression e)

let rec generate_asm_statement ?retlbl varl sp s il =
  match s with
  | ReturnStmt None ->
      il 
        |+ "addq $" ^ string_of_int sp ^ ", %rsp" 
        |+ "popq %rbp" 
        |+ "retq"
  (* Todo *)
  |CompoundStmt( head_var::tail_var,h::t)->
      let sp2 =sp+8 in let addr="-"^string_of_int sp2^"(%rbp)"
      in let varl2= (head_var,addr) :: varl in 
      let il2=
      il|+"subq $8, %rsp"
      in let next=CompoundStmt( tail_var,h::t)
      in generate_asm_statement varl2 sp2 next il2|+"addq $8, %rsp"
  | CompoundStmt ([],h::t)->
      let il2 = generate_asm_statement varl sp h il 
          in let next=CompoundStmt ([],t) 
          in generate_asm_statement varl sp next il2
  | CompoundStmt (_,[])-> il 
  | ReturnStmt (Some expr) ->
    let il2 = generate_asm_expression varl sp expr il
    in let return=ReturnStmt (None)
    in generate_asm_statement varl sp return il2
  |IfStmt (expression , state_if, state_else)->
    let  else_label= fresh_lbl "L" in let end_label =fresh_lbl "L"
  in  let il2 = generate_asm_expression varl sp expression  il 
      |+ "testq %rax , %rax" |+"jz "^else_label 
  in  let il3= generate_asm_statement   varl sp state_if il2 
      |+ "jmp " ^end_label
      |+ else_label^":"
  in  generate_asm_statement   varl sp state_else il3 |+end_label^":"
  | WhileStmt _ ->raise (Code_gen_failure_statment s)
  | Expr (expression)->generate_asm_expression  varl sp expression il

let register_arg=["%rdi"; "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9"]

let rec declare sp varl il arg_list =(*fonction auxiliaires pour les variables*)
  match arg_list with 
   |[]->(sp,varl,il)
   |h::t->let (sp2,varl2,il2)=declare sp varl il t
    in
    let sp3 =sp2+8 in let addr="-"^string_of_int sp3^"(%rbp)"
    in let nvarl= (h,addr) :: varl2 in 
      let il3=
      il2|+"subq $8, %rsp"
        |+"movq" ^(List.nth  register_arg  (sp3/8)) ^"  ,(%rsp)"


  in (sp3 ,nvarl, il3)
    
let generate_asm_top varl il decl =
  match decl with
  | FunctionDecl (f, arg_list, s) ->
      let il2 = il 
        |+ f ^ ":" 
        |+ "pushq %rbp" 
        |+ "movq %rsp, %rbp"
      in  let (sp ,varl,il2) =  declare 0 varl il2 arg_list (* renvoies un triplet*)
       in
            (* Todo*)
            generate_asm_statement varl sp s il2
            |+ "addq $"^string_of_int sp^", %rsp" 
            |+ "popq %rbp"
            |+"retq"

            
           (**failwith "TO DO"*)

  | VarDecl _ -> il
(* les variables globals sont déjà geré dans le fichier compilo.ml.
   On ne fait donc rien ici. *)
