open ASMType

type 'a graph = ('a * int list * int option) array

let pcol f = function None -> () | Some s -> Format.fprintf f "%i" s

let to_dot f p (g : 'a graph) =
  Format.fprintf f "digraph{@.";
  Array.iteri
    (fun i (v, _, col) ->
      Format.fprintf f "%i[label=\"%a:%a\"];@." i p (i, v) pcol col)
    g;
  Array.iteri
    (fun i (_, arrete, _) ->
      List.iter (fun j -> Format.fprintf f "%i->%i@." i j) arrete)
    g;
  Format.fprintf f "}@."

let print_life f reg =
  List.iter (fun (i, w, r) -> Format.fprintf f "%%m%i->[%i,%i]@." i w r) reg

let rec update_list f g i = function
  | [] -> g ()
  | (j, x, y) :: q when j = i -> f (j, x, y) :: q
  | t :: q -> t :: update_list f g i q

let add_read ind arg reg =
  match arg with
  | R (M i) ->
      update_list
        (fun (_, w, _) -> (i, w, ind))
        (fun () -> failwith ("unknown read" ^ string_of_int i))
        i reg
  | _ -> reg

let add_write ind arg reg =
  match arg with
  | R (M i) ->
      update_list
        (fun (_, w, _) -> (i, w, ind))
        (fun () -> [ (i, ind, ind) ])
        i reg
  | _ -> reg

let compute_life_aux i reg inst =
  let re a = add_read i a reg in
  let wr a = add_write i a reg in
  match inst with
  | MOV (arg1, arg2)
  | ADD (arg1, arg2)
  | MUL (arg1, arg2)
  | SUB (arg1, arg2)
  | DIV (arg1, arg2)
  | XOR (arg1, arg2)
  | OR (arg1, arg2)
  | AND (arg1, arg2)
  | CMP (arg1, arg2)
  | TEST (arg1, arg2)
  | LEAQ (arg1, arg2) ->
      reg |> add_read i arg1 |> add_write i arg2
  | PUSH arg | Label arg | JMP arg | JZ arg | JNZ arg | CALL arg -> re arg
  | POP arg | NEG arg | NOT arg | SETE arg | SETNE arg | SETNS arg | SETG arg ->
      wr arg
  | NOP | RET | StrLit _ | GlobVar _ | LinkInstr _ -> reg

let compute_life insts =
  snd
  @@ List.fold_left
       (fun (i, reg) inst -> (i + 1, compute_life_aux i reg inst))
       (0, []) insts

let compute_graph reg =
  let rega = Array.make (List.length reg) (0, 0) in
  List.iter (fun (i, w, r) -> rega.(i) <- (w, r)) reg;
  Array.mapi
    (fun i (w, r) ->
      ( (),
        reg
        |> List.filter (fun (j, w2, r2) -> not (w2 > r || w > r2 || i = j))
        |> List.map (fun (j, _, _) -> j),
        None ))
    rega

let explore g c q i =
  let v, neighbour, _ = g.(i) in
  let acol =
    List.fold_left
      (fun cit j ->
        match g.(j) with
        | _, _, None -> cit
        | _, _, Some col -> List.filter (fun x -> x <> col) cit)
      c neighbour
  in
  match acol with
  | [] -> failwith "not enough color"
  | t :: _ ->
      g.(i) <- (v, neighbour, Some t);
      List.iter
        (fun j -> match g.(j) with _, _, None -> Queue.add j q | _ -> ())
        neighbour

let colorize g c =
  let q = Queue.create () in
  Array.iteri
    (fun i (_, _, v) ->
      match v with
      | None ->
          Queue.add i q;
          while not (Queue.is_empty q) do
            let i = Queue.pop q in
            explore g c q i
          done
      | _ -> ())
    g;
  ()
