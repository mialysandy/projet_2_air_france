(**
  Module principal. Compile du code C sous forme d'AST décrit dans le module {!CLessType}
 en du code assembleur décris dans le module {!ASMType} par le type {!ASMType.asm_program} .
 *)

type environnement = (string * string) list
(**
Le type [environnement] represente les variables déclarées dans le programme (local et global) , c'est une liste
de paire contenant une chaine de caratère et une addresse mémoire sous forme d'un argument assembleur.
 *)

val generate_asm_expression :
  environnement ->
  int ->
  CLessType.expression ->
  ASMType.asm_program ->
  ASMType.asm_program
(**
[generate_asm_expression varl sp e p] génère les instructions implémentant l'expression [e] et l'ajoute
    au programme [p] passé en paramètre.
  - [varl] est l'environnement;
  - [sp] est la distance en octet entre le sommet et la base de la pile;
  - [e] est l' expression à compiler;
  - [p] est le programme auquel la fonction ajoute le code compilé.
*)

val generate_asm_statement :
  ?retlbl:string ->
  environnement ->
  int ->
  CLessType.statement ->
  ASMType.asm_program ->
  ASMType.asm_program
(**
[generate_asm_statement ?retlbl varl sp s p] génère les instructions implémentant le statement [s] et l'ajoute
    au programme [p] passé en paramètre.
  - [retlbl] est une continuation sous forme de label cette argument peut etre ignoré au début (le ? indique qu'il peut etre absent à l'appel de la fonction);
  - [varl] est l'environnement;
  - [sp] est la distance en octet entre le sommet et la base de la pile;
  - [s] est le statement à compiler;
  - [p] est le programme auquel la fonction ajoute le code compilé.
*)

val generate_asm_top :
  environnement ->
  ASMType.asm_program ->
  CLessType.declaration ->
  ASMType.asm_program
(**
[generate_asm_top varl p dec] génère les instructions implémentant la déclaration [dec] et l'ajoute
    au programme [p] passé en paramètre.
  - [varl] est l'environnement, ici il ne contient que des variables global;
  - [p] est le programme auquel la fonction ajoute le code compilé;
  - [dec] est la déclaration à compiler.
*)
