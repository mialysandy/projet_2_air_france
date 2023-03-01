(** AST d'un sous-langage de C *)

(**/**)

(* Position du parseur pour reporter les erreurs; à ignorer *)
let cline = ref 1
let ccol = ref 0
let oldcline = ref 0
let oldccol = ref 0
let cfile = ref "stdin"
let fatal _ s = failwith s

(**/**)

(** {2 Définitions des types de l'arbre de syntaxique abstrait} *)

(** Type des opérateurs unaire *)
type uop =
  | Not  (** négation booléenne *)
  | MinusM  (** moins unaire i.e. -x *)
  | Deref  (** déréference i.e. *x *)

(** Type des opérateurs binaires *)
type bop =
  | Mult  (** multiplication i.e. [*] *)
  | Add  (** addition i.e. [+] *)
  | Div  (** division i.e. [/] *)
  | Sub  (** soustraction i.e. [-] *)
  | Mod  (** modulo i.e. [%] *)
  | And  (** et booléen i.e. [&&] *)
  | Or  (** ou booléen i.e. [||] *)
  | EQ  (** égale i.e. [==] *)
  | NEQ  (** différent i.e. [!=] *)
  | LE  (** plus petit ou égale i.e. [<=] *)
  | LL  (** strictement plus petit i.e. [<] *)
  | Index  (**gestion des tableaux *)
  | SetReference  (** assigne une valeur à une reference i.e. [&x = y] *)

(** Type des expressions C *)
type expression =
  | Var of string  (** nom d'une variable *)
  | IntegerLiteral of int  (** entier littéral, i.e. constante *)
  | Set of string * expression
      (** assigement de variable i.e. [Set("x",e)] -> x=e; *)
  | Call of string * expression list
      (** appel de fonction i.e. [Call("f",[e1;e2; ...])] -> f(e1,e2,...) *)
  | UOp of uop * expression  (** application d'opérateur unaire *)
  | BOp of expression * bop * expression  (** application d'opérateur binaire *)
  | Seq of expression list
      (** séquence d'expression i.e. [Seq([e1;e2;...])] -> e1;e2;... *)
  | StringLiteral of string
      (** string littéral i.e. chaine de caractères constante *)
  | SetArray of string * expression * expression
      (** Assigement de tableau i.e. [SetArray("x",i,e)] -> x[i]=e; *)
  | Ref of string  (** get a reference i.e. &x*)

(** Type des statement en C *)
type statement =
  | CompoundStmt of string list * statement list
      (** bloc: liste de déclaration de variable locale
et liste de statement *)
  | Expr of expression  (** une expression *)
  | IfStmt of expression * statement * statement
      (** bloc if-then-else: [IfStmt(e,s1,s2)] -> if(e)\{s1\}else\{s2\} *)
  | WhileStmt of expression * statement
      (** bloc while: [WhileStmt(e,s)] -> while(e)\{s\} *)
  | ReturnStmt of expression option  (** retour d'une fonction *)

type declaration =
  | VarDecl of string * int option  (** définition de variable global *)
  | FunctionDecl of string * string list * statement
      (** définition de fonction *)

(** {2 Iterateurs génériques sur les arbres} *)

(** Iterateur sur les expressions *)
let rec iter_fun_expr f e =
  match f e with
  | Some x -> x
  | None -> (
      match e with
      | UOp (mop, e) -> UOp (mop, iter_fun_expr f e)
      | BOp (e1, bop, e2) -> BOp (iter_fun_expr f e1, bop, iter_fun_expr f e2)
      | Set (s, e) -> Set (s, iter_fun_expr f e)
      | Seq el -> Seq (List.map (iter_fun_expr f) el)
      | Var x -> Var x
      | IntegerLiteral i -> IntegerLiteral i
      | StringLiteral s -> StringLiteral s
      | Call (fn, el) -> Call (fn, List.map (iter_fun_expr f) el)
      | Ref x -> Ref x
      | SetArray (s, i, e) -> SetArray (s, iter_fun_expr f i, iter_fun_expr f e)
      )

(** Iterateur sur les statements *)
let rec iter_fun_stat f g s =
  match g s with
  | Some x -> x
  | None -> (
      match s with
      | CompoundStmt (dl, sl) ->
          CompoundStmt (dl, List.map (iter_fun_stat f g) sl)
      | Expr e -> Expr (iter_fun_expr f e)
      | IfStmt (e, s1, s2) ->
          IfStmt (iter_fun_expr f e, iter_fun_stat f g s1, iter_fun_stat f g s2)
      | WhileStmt (e, s) -> WhileStmt (iter_fun_expr f e, iter_fun_stat f g s)
      | ReturnStmt (Some e) -> ReturnStmt (Some (iter_fun_expr f e))
      | ReturnStmt None -> ReturnStmt None)

let iter_fun_top f g = function
  | VarDecl (n, v) -> VarDecl (n, v)
  | FunctionDecl (fn, al, s) -> FunctionDecl (fn, al, iter_fun_stat f g s)
