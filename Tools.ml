(** Fonctions d'aide à la compilation *)

open CLessType
open ASMType

(** Ce module contient des fonctions d'aide à la compilation qui pourrons vous être utile. *)

(** {2 Exceptions pendant la génération de code } *)

exception Code_gen_failure_expression of expression
(** L'exception [Code_gen_failure_expression] doit être levée lorsqu’une expression ne peut pas être compilée.
Cette exception est rattrapée par le compilateur pour afficher un message d'erreur.*)

exception Code_gen_failure_statment of statement
(** L'exception [Code_gen_failure_statement] doit être levée lorsqu’un statement ne peut pas être compiler.
Cette exception est rattrapée par le compilateur pour afficher un message d'erreur.*)

(**/**)

(** {2 Fonctions de manipulation d'assembleur } *)

(** Le compilateur utilise une représentation symbolique d'un programme assembleur ({!ASMType.asm_program})
plutôt que sous forme de chaine de caractère pour permettre d'écrire des optimisations du code généré.

Les fonctions suivantes permettent de parser des instructions assembleur décrites par des chaines de caractères et de renvoyer une représentation de l'instruction.
 *)

let parse_all s i a =
  let lexbuf = Lexing.from_string s in
  try
    let defList = ASMGram.instruction ASMLex.asmtoken lexbuf in
    replace_holl defList a HollReg i ""
  with Parsing.Parse_error ->
    let p = string_of_int lexbuf.Lexing.lex_curr_pos in
    failwith
      ("Unexpected token in '" ^ s ^ ":"
      ^ Bytes.to_string lexbuf.Lexing.lex_buffer
      ^ "' at " ^ p)

(**/**)

(*
   (** [p s] renvoie l'instruction assembleur décrite par [s].

    Ex d'utilisation [ p "movq $3, %rax" ]. *)
   let p s = parse_all s 0 (C (IL(0)))
*)

(** {2 Fonctions d'aide à la compilation } *)

(**/**)

(* [ q |% t ] renvoie une liste dont la tête est [t] et la queue est [q] (i.e. [t::q])*)
let ( |% ) l i = i :: l

(**/**)

(** [ x |> f ] renvoie l'évaluation de [f] sur [ x ] (i.e. [f x]) *)
let ( |> ) x f = f x

(** [ l |+ s ]  renvoie une liste dont la tête est [ s ] transformé en instruction assembleur et la queue est [l]. Cela permet d'ajouter des instructions à la liste d'instruction, avec la fonction {!val:ASMType.add_to_prog} c'est la seul maniere d'ajouter une instruction à un programme *)
let ( |+ ) l s = add_to_prog (parse_all s 0 (C (IL 0))) l

(*
(** [pi s i] renvoie l'instruction assembleur décrit par [s] où les occurrences de '%i' sont remplacé par [i].

Ex d'utilisation [ pi "movq %i, %rax" 3 ]. *)
let pi s i = parse_all s i (C (IL(0)))

(** [parse_arg s i] renvoie l'argument d'une instruction assembleur décrit par [s] où les occurrences de '%i' sont remplacé par [i]. 

Ex d'utilisation [ parse_arg "-%i(%rsp)" 8 ] représente l'adresse pointée par [%rsp-8] . *)
let parse_arg s i =
  let lexbuf = Lexing.from_string s in
  try
    let defList = ASMGram.argument ASMLex.asmtoken lexbuf in
    replace_holl_arg HollReg i "" defList 
  with
    Parsing.Parse_error ->
    let p = string_of_int lexbuf.Lexing.lex_curr_pos in
    failwith ("Unexpected token in '" ^ (Bytes.to_string (lexbuf.Lexing.lex_buffer)) ^ "' at "^p) 
                       
(** [pa s a] renvoie l'instruction assembleur décrite par [s] ou les occurrences de '%a' sont remplacé par [a].

 Ex d'utilisation [ pa "movq %a, %rax" (parse_arg "-%i(%rsp)" 8) ] déplace l'adresse pointée par [%rsp-8] dans [%rax]. *)             
let pa s a = parse_all s 0 a

(** [pr s r] renvoie l'instruction assembleur décrite par [s] ou les occurrences de '%r' sont remplacé par le registre [r]. *)                    
let pr s r = parse_all s 0 (R r)
*)

(**/**)

let labcount = ref (-1)

(**/**)

(** Retourne un nom unique contenant la chaine de caractère donnée en argument *)
let fresh_lbl s =
  incr labcount;
  s ^ "_" ^ string_of_int !labcount

(**/**)

let string_tab : (string, string) Hashtbl.t = Hashtbl.create 10

(**/**)

(** Associe à chaque chaîne de caractère littéral une adresse. Utiles pour gérer les chaines de caractère littéral. *)
let addr_lbl_of_string s =
  let label =
    try Hashtbl.find string_tab s
    with Not_found ->
      let sl = fresh_lbl "string" in
      Hashtbl.add string_tab s sl;
      sl
  in
  label ^ "(%rip)"

(**/**)

module StringMap = Map.Make (String)

let funDecList = ref StringMap.empty

(**/**)

(** [getFunDec s] renvoi la définition de la fonction de nom [s] *)
let getFunDec (f : string) : string list * statement =
  StringMap.find f !funDecList
