(** Exemple d'optimisation simple *)

open ASMType

let is_direct = function C _ | L _ | R _ -> true | _ -> false

let rec removePopPush = function
  | [] -> []
  | ADD (C (IL 8), R RSP) :: PUSH x :: l when is_direct x ->
      MOV (x, ARI (IL 0, RSP)) :: removePopPush l
  | MOV (C x, R r1) :: PUSH (R r2) :: l when r1 = r2 ->
      PUSH (C x) :: removePopPush l
  | PUSH a :: POP (R r) :: l -> MOV (a, R r) :: removePopPush l
  | MOV (x, R r1) :: MOV (R r2, R r3) :: l when r1 = r2 ->
      removePopPush (MOV (x, R r3) :: l)
  | ADD (C (IL x1), R r1) :: ADD (C (IL x2), R r2) :: l when r1 = r2 ->
      removePopPush (ADD (C (IL (x1 + x2)), R r1) :: l)
  | ADD (C (IL x1), R r1) :: SUB (C (IL x2), R r2) :: l when r1 = r2 ->
      removePopPush (ADD (C (IL (x1 + x2)), R r1) :: l)
  | SUB (C (IL x1), R r1) :: ADD (C (IL x2), R r2) :: l when r1 = r2 ->
      removePopPush (ADD (C (IL (x1 + x2)), R r1) :: l)
  | SUB (C (IL x1), R r1) :: SUB (C (IL x2), R r2) :: l when r1 = r2 ->
      removePopPush (SUB (C (IL (x1 + x2)), R r1) :: l)
  | LEAQ (arg, R r1) :: MOV (R r2, R r3) :: l when r1 = r2 ->
      removePopPush (LEAQ (arg, R r3) :: l)
  | MOV (a, b) :: MOV (c, d) :: l when a = d && b = c ->
      removePopPush (MOV (a, b) :: l)
  | JMP (L l1) :: Label (L l2) :: l when l1 = l2 ->
      removePopPush (Label (L l2) :: l)
  | x :: l -> x :: removePopPush l

let rec simplifyMov = function
  | [] -> []
  | MOV (v1, vp1) :: MOV (vp2, v2) :: MOV (v3, vp3) :: l
    when vp1 = vp2 && vp1 = vp3
         && (is_direct v1 || is_direct v2)
         && (is_direct v3 || is_direct vp3) ->
      MOV (v1, v2) :: simplifyMov (MOV (v3, vp3) :: l)
  | MOV (R r1, a1) :: MOV (b, R r2) :: MUL (a2, R r3) :: MOV (c, a3) :: l
    when r1 = r2 && r2 = r3 && a1 = a2 && a2 = a3
         && (is_direct c || is_direct a1) ->
      simplifyMov (MUL (b, R r1) :: MOV (c, a1) :: l)
  | MOV (R r1, ARI (IL 0, RSP))
    :: MOV (b, R r2)
    :: MUL (ARI (IL 0, RSP), R r3)
    :: ADD (C (IL i), R RSP)
    :: l
    when r1 = r2 && r2 = r3 && i >= 8 ->
      simplifyMov (MUL (b, R r1) :: ADD (C (IL i), R RSP) :: l)
  | MOV (R r1, a1) :: MOV (b, R r2) :: ADD (a2, R r3) :: MOV (c, a3) :: l
    when r1 = r2 && r2 = r3 && a1 = a2 && a2 = a3
         && (is_direct c || is_direct a1) ->
      simplifyMov (ADD (b, R r1) :: MOV (c, a1) :: l)
  | MOV (R r1, ARI (IL 0, RSP))
    :: MOV (b, R r2)
    :: ADD (ARI (IL 0, RSP), R r3)
    :: ADD (C (IL i), R RSP)
    :: l
    when r1 = r2 && r2 = r3 && i >= 8 ->
      simplifyMov (ADD (b, R r1) :: ADD (C (IL i), R RSP) :: l)
  | x :: l -> x :: simplifyMov l

let rec removeZeroAdd = function
  | [] -> []
  | ADD (C (IL 0), _) :: li -> removeZeroAdd li
  | SUB (C (IL 0), _) :: li -> removeZeroAdd li
  | x :: li -> x :: removeZeroAdd li

let rec removeDeadCode = function
  | [] -> []
  | RET :: q -> RET :: findLabel q
  | JMP x :: q -> JMP x :: findLabel q
  | x :: q -> x :: removeDeadCode q

and findLabel = function
  | [] -> []
  | Label x :: q -> Label x :: removeDeadCode q
  | _ :: q -> findLabel q

let optimise il =
  simplifyMov (removeDeadCode (removePopPush (removeZeroAdd il)))

let rec apply n f x = if n <= 0 then x else apply (n - 1) f (f x)
