open CLessType
open Format

(** Fonction d'impression pour le débogage de [CLessType] sous forme d'AST*)

let print_mop o = function
  | Not -> fprintf o "Not"
  | MinusM -> fprintf o "MinusM"
  | Deref -> fprintf o "Deref"

let print_dot_mop o = function
  | Not -> Printf.fprintf o "Not"
  | MinusM -> Printf.fprintf o "MinusM"
  | Deref -> Printf.fprintf o "Deref"

let print_bop o = function
  | Mult -> fprintf o "Mult"
  | Add -> fprintf o "Add"
  | Div -> fprintf o "Div"
  | Sub -> fprintf o "Sub"
  | Mod -> fprintf o "Mod"
  | And -> fprintf o "And"
  | Or -> fprintf o "Or"
  | EQ -> fprintf o "EQ"
  | NEQ -> fprintf o "NEQ"
  | LE -> fprintf o "LE"
  | LL -> fprintf o "LL"
  | Index -> fprintf o "Index"
  | SetReference -> fprintf o "SetReference"

let print_dot_bop o = function
  | Mult -> Printf.fprintf o "*"
  | Add -> Printf.fprintf o "+"
  | Div -> Printf.fprintf o "/"
  | Sub -> Printf.fprintf o "-"
  | Mod -> Printf.fprintf o "mod"
  | And -> Printf.fprintf o "And"
  | Or -> Printf.fprintf o "Or"
  | EQ -> Printf.fprintf o "=="
  | NEQ -> Printf.fprintf o "!="
  | LE -> Printf.fprintf o "<="
  | LL -> Printf.fprintf o "<"
  | Index -> Printf.fprintf o "[ . ]"
  | SetReference -> Printf.fprintf o "SetReference"

let rec print_list sep f o = function
  | [] -> ()
  | [ h ] -> f o h
  | h :: t ->
      f o h;
      fprintf o sep;
      print_list sep f o t

let rec print_dot_list sep o = function
  | [] -> ()
  | [ h ] -> output_string o h
  | h :: t ->
      output_string o h;
      Printf.fprintf o sep;
      print_dot_list sep o t

let print_tab o tab =
  let s = String.make tab ' ' in
  fprintf o "%s" s

let rec print_list_clang pre f o = function
  | [] -> ()
  | [ t ] -> f pre '`' o t
  | t :: q ->
      f pre '-' o t;
      print_list_clang pre f o q

(*let rec print_clang_expression pre c o = function
  | Var s -> fprintf o  "%s%c-Var \"%s\"\n" pre c s
  | Ref s -> fprintf o "Ref \"%s\"" s
  | IntegerLiteral i -> fprintf o  "%s%c-IntegerLiteral %i\n" pre c i
  | Set(s,e) -> fprintf o "%s%c-Set\n%s  |-RefVar \"%s\"\n%a" pre c pre s (print_clang_expression (pre) '`') e
  | Call(s,el) -> fprintf o "%s%c-Call \"%s\"\n%a" pre c s (print_list_clang (pre^" |") print_clang_expression) el
  | UnaryOperator(m,e) ->
     fprintf o "%s%c-UnaryOperator %a\n%a" pre c
             print_mop m
             (print_clang_expression (pre^"  ") '`') e
  | BOperator(e,Index,i) ->
     fprintf o "%a[%a]" print_expression e
       print_expression i
  | BinaryOperator(e1,b,e2) ->
     fprintf o "%s%c-BinaryOperator %a\n%a%a" pre c print_bop b (print_clang_expression (pre^" |") `-` ) e1
                    print_bop b
                    print_expression e2
  | Seq(el) -> fprintf o "[%a]" (print_list ";@;" print_expression) el
  | StringLiteral(s) -> fprintf o ("\"%s\"") (String.escaped s)
*)

let rec print_expression o = function
  | Var s -> fprintf o "@[<hv 2>Var@ \"%s\"@]" s
  | Ref s -> fprintf o "Ref \"%s\"" s
  | IntegerLiteral i -> fprintf o "@[<hv 2>IntegerLiteral@ %i@]" i
  | Set (s, e) -> fprintf o "@[<hv 2>Set@ (\"%s\",%a)@]" s print_expression e
  | Call (s, el) ->
      fprintf o "@[<hv 2>Call@ (\"%s\",[%a])@]" s
        (print_list "," print_expression)
        el
  | UOp (m, e) ->
      fprintf o "@[<hv 2>UOp@ (%a,%a)@]" print_mop m print_expression e
  | BOp (e, Index, i) ->
      fprintf o "%a[%a]" print_expression e print_expression i
  | BOp (e1, b, e2) ->
      fprintf o "@[<hv 2>BOp@ (%a,%a,%a)@]" print_expression e1 print_bop b
        print_expression e2
  | Seq el -> fprintf o "[%a]" (print_list ";@;" print_expression) el
  | StringLiteral s -> fprintf o "\"%s\"" (String.escaped s)
  | SetArray (s, i, e) ->
      fprintf o "%s[%a]=%a" s print_expression i print_expression e

let perror_exp e =
  fprintf err_formatter "Fail to generate %a@\n@?" print_expression e

let rec print_dot_expression o id = function
  | Var s ->
      Printf.fprintf o "id%i [label=\"%s\"]\n" (id + 1) s;
      id + 1
  | Ref s ->
      Printf.fprintf o "Ref \"%s\"" s;
      id + 1
  | IntegerLiteral i ->
      Printf.fprintf o "id%i [label=\"%i\"]\n" (id + 1) i;
      id + 1
  | StringLiteral s ->
      Printf.fprintf o "id%i [label=\"%s\"]\n" (id + 1) (String.escaped s);
      id + 1
  | Set (s, e) ->
      let ide = print_dot_expression o id e in
      Printf.fprintf o "id%i [label=\"Set %s\"]\nid%i -> id%i\n" (ide + 1) s
        (ide + 1) ide;
      ide + 1
  | UOp (m, e) ->
      let ide = print_dot_expression o id e in
      Printf.fprintf o "id%i [label=\"UOp %a\"]\n" (ide + 1) print_dot_mop m;
      Printf.fprintf o "id%i -> id%i\n" (ide + 1) ide;
      ide + 1
  | BOp (e1, b, e2) ->
      let ide1 = print_dot_expression o id e1 in
      let ide2 = print_dot_expression o ide1 e2 in
      Printf.fprintf o "id%i [label=\"%a\"]\n" (ide2 + 1) print_dot_bop b;
      Printf.fprintf o "id%i -> id%i\nid%i -> id%i\n" (ide2 + 1) ide1 (ide2 + 1)
        ide2;
      ide2 + 1
  | Call (s, sl) ->
      let i3, l =
        List.fold_left
          (fun (i, nl) stat ->
            let i2 = print_dot_expression o i stat in
            (i2, i2 :: nl))
          (id, []) sl
      in
      Printf.fprintf o "id%i [label=\"Call %s\"]\n" (i3 + 1) s;
      List.iter (fun x -> Printf.fprintf o "id%i -> id%i\n" (i3 + 1) x) l;
      i3 + 1
  | Seq sl ->
      let i3, l =
        List.fold_left
          (fun (i, nl) stat ->
            let i2 = print_dot_expression o i stat in
            (i2, i2 :: nl))
          (id, []) sl
      in
      Printf.fprintf o "id%i [label=\"Seq\"]\n" (i3 + 1);
      List.iter (fun x -> Printf.fprintf o "id%i -> id%i\n" (i3 + 1) x) l;
      i3 + 1
  | SetArray (_, _, e) ->
      let ide2 = print_dot_expression o id e in
      ide2

let print_escaped o s = fprintf o "\"%s\"" s

let rec print_declaration o = function
  | VarDecl (s, None) -> fprintf o "@[<hv 2>VarDecl@ \"%s\"@]" s
  | VarDecl (s, Some i) -> fprintf o "@[<hv 2>VarDecl@ \"%s=%i\"@]" s i
  | FunctionDecl (s, dl, stat) ->
      fprintf o "@[<hv 2>FunctionDecl(\"%s\",[%a],%a)@]" s
        (print_list ";" print_escaped)
        dl print_statement stat

and print_statement o = function
  | Expr e -> fprintf o "@[<hv 2>Expr(%a)@]" print_expression e
  | CompoundStmt (dl, sl) ->
      fprintf o "@[<hv 2>CompoundStmt([%a],@,[%a])@]"
        (print_list ";@;" print_escaped)
        dl
        (print_list ";@;" print_statement)
        sl
  | IfStmt (e, s1, s2) ->
      fprintf o "@[<hv 2>IfStmt(%a,@,%a,@,%a)@]" print_expression e
        print_statement s1 print_statement s2
  | WhileStmt (e, s) ->
      fprintf o "@[<hv 2>WhileStmt(%a,@,%a)@]" print_expression e
        print_statement s
  | ReturnStmt None -> fprintf o "@[<hv 2>ReturnStmt@ (None)@]"
  | ReturnStmt (Some s) ->
      fprintf o "@[<hv 2>ReturnStmt@ (Some@ (%a))@]" print_expression s

let perror_stat e =
  fprintf err_formatter "Fail to generate %a@\n@?" print_statement e

let rec print_dot_dec o id = function
  | VarDecl (s, None) ->
      Printf.fprintf o "id%i [label=\"Var %s\",shape=diamond]\n" (id + 1) s;
      id + 1
  | VarDecl (s, Some i) ->
      Printf.fprintf o "id%i [label=\"Var %s=%i\",shape=diamond]\n" (id + 1) s i;
      id + 1
  | FunctionDecl (s, dl, stat) ->
      Printf.fprintf o "id%i [label=\"FunctionDecl %s(%a)\",shape=diamond]\n"
        (id + 1) s (print_dot_list ";") dl;
      let idstat = print_dot_stat o (id + 1) stat in
      Printf.fprintf o "id%i -> id%i\n" (id + 1) idstat;
      idstat

and print_dot_stat o id = function
  | Expr e ->
      let ide = print_dot_expression o id e in
      Printf.fprintf o "id%i [label=\"Expr\",shape=box]\nid%i -> id%i\n"
        (ide + 1) (ide + 1) ide;
      ide + 1
  | ReturnStmt None ->
      Printf.fprintf o "id%i [label=\"Return\",shape=box]\n" (id + 1);
      id + 1
  | ReturnStmt (Some e) ->
      let ide = print_dot_expression o id e in
      Printf.fprintf o "id%i [label=\"Return\",shape=box]\nid%i -> id%i\n"
        (ide + 1) (ide + 1) ide;
      ide + 1
  | CompoundStmt (dl, sl) ->
      let i3, l =
        List.fold_left
          (fun (i, nl) stat ->
            let i2 = print_dot_stat o i stat in
            (i2, i2 :: nl))
          (id, []) sl
      in
      Printf.fprintf o "id%i [label=\"CompoundStmt(%a)\",shape=box]\n" (i3 + 1)
        (print_dot_list ",") dl;
      List.iter (fun x -> Printf.fprintf o "id%i -> id%i\n" (i3 + 1) x) l;
      i3 + 1
  | IfStmt (e, s1, s2) ->
      let ide = print_dot_expression o id e in
      let ids1 = print_dot_stat o ide s1 in
      let ids2 = print_dot_stat o ids1 s2 in
      Printf.fprintf o "id%i [label=\"If\",shape=box]\n" (ids2 + 1);
      Printf.fprintf o "id%i -> id%i\nid%i -> id%i\nid%i -> id%i\n" (ids2 + 1)
        ide (ids2 + 1) ids1 (ids2 + 1) ids2;
      ids2 + 1
  | WhileStmt (e, s) ->
      let ide = print_dot_expression o id e in
      let ids = print_dot_stat o ide s in
      Printf.fprintf o "id%i [label=\"While\",shape=box]\n" (ids + 1);
      Printf.fprintf o "id%i -> id%i\nid%i -> id%i\n" (ids + 1) ide (ids + 1)
        ids;
      ids + 1

let print_dec_list o dl =
  fprintf o "[@[<hv 2>@;%a@;@]]@." (print_list ";@;" print_declaration) dl

let print_dot_dec_list o dl = List.fold_left (print_dot_dec o) 0 dl
