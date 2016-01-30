module PrimList = List

(* TODO:
- addr type (where does this need to be injected?)
- split
- 
*)
(*
type addr = int
let alloc : int * heap -> (int * heap) =
  fun (i, h) ->
    let n = *)

open Adapton
module IntList = Types.List(Types.Int)

module Make(ArtLib : ArtLib.S) = struct

include Spaces.Make(ArtLib)

type aexpr =
  | Int of int
  | Plus of aexpr * aexpr
  | Minus of aexpr * aexpr
  | Times of aexpr * aexpr
  | Div of aexpr * aexpr
  | Var of string
  | Alloc of aexpr
  | Get of aexpr * aexpr
[@@deriving eq, ord, show]

type bexpr =
  | True
  | False
  | Not of bexpr
  | And of bexpr * bexpr
  | Or of bexpr * bexpr
  | Eq of aexpr * aexpr
  | Leq of aexpr * aexpr
[@@deriving eq, ord, show]

type cmd =
  | Skip
  | Assign of string * aexpr
  | Set of aexpr * aexpr * aexpr
  | Seq of cmd * cmd
  | If of bexpr * cmd * cmd
  | While of bexpr * cmd
  | AWhile of aexpr * bexpr * cmd
[@@deriving eq, ord, show]

let rec string_of_aexpr : aexpr -> string = function
  |    Var  x           -> x
  |    Int  i           -> string_of_int i
  |   Plus (a0, a1)     -> Printf.sprintf "(+ %s %s)" (string_of_aexpr a0) (string_of_aexpr a1)
  |  Minus (a0, a1)     -> Printf.sprintf "(- %s %s)" (string_of_aexpr a0) (string_of_aexpr a1)
  |  Times (a0, a1)     -> Printf.sprintf "(* %s %s)" (string_of_aexpr a0) (string_of_aexpr a1)
  |    Div (a0, a1)     -> Printf.sprintf "(/ %s %s)" (string_of_aexpr a0) (string_of_aexpr a1)
  |  Alloc  a           -> Printf.sprintf "(alloc %s)" (string_of_aexpr a)
  |    Get (a0, a1)     -> Printf.sprintf "(get %s %s)" (string_of_aexpr a0) (string_of_aexpr a1)
let string_of_cmd : cmd -> string =
  let rec string_of_bexpr : bexpr -> string = function
    | True         -> "true"
    | False        -> "false"
    | Not b        -> Printf.sprintf "!%s" (string_of_bexpr b)
    | And (b0, b1) -> Printf.sprintf "(and %s %s)" (string_of_bexpr b0) (string_of_bexpr b1)
    |  Or (b0, b1) -> Printf.sprintf "(or %s %s)"  (string_of_bexpr b0) (string_of_bexpr b1)
    |  Eq (a0, a1) -> Printf.sprintf "(= %s %s)"   (string_of_aexpr a0) (string_of_aexpr a1)
    | Leq (a0, a1) -> Printf.sprintf "(<= %s %s)"  (string_of_aexpr a0) (string_of_aexpr a1)
  in
  let rec loop : cmd -> string = function
    |      Skip              -> "(skip)"
    |    Assign (x, a)       -> Printf.sprintf "(:= %s %s)" x (string_of_aexpr a)
    |       Set (a0, a1, a2) -> Printf.sprintf "(set %s %s %s)" (string_of_aexpr a0) (string_of_aexpr a1) (string_of_aexpr a2)
    |       Seq (c0, c1)     -> Printf.sprintf "%s ;\n%s" (loop c0) (loop c1)
    |        If (b, c0, c1)  -> Printf.sprintf "(if %s\n%s\n%s)" (string_of_bexpr b) (loop c0) (loop c1)
    |     While (b, c)       -> Printf.sprintf "(while %s\n%s)" (string_of_bexpr b) (loop c)
  in loop

module CmdP =
struct
  type 'a t =
    | Skip
    | Assign of Name.t * string * aexpr
    | Seq of 'a t * 'a t
    | If of bexpr * 'a t * 'a t
    | While of Name.t * bexpr * 'a t
    | Set of Name.t * aexpr * aexpr * aexpr
    (* Boilerplate cases: *)
    | Art of 'a
    | Name of Name.t * 'a t
  [@@deriving eq, ord, show]
  let rec sanitize san x = match x with
    | Skip               -> x
    | Assign (nm, st, a) -> Assign (nm, st, a)
    | Seq (t, t')        -> Seq (sanitize san t, sanitize san t')
    | If (b, t, t')      -> If (b, sanitize san t, sanitize san t')
    | While (nm, b, t)   -> While (Name.sanitize nm, b, sanitize san t)
    | Set  (nm, a, b, c) -> Set (Name.sanitize nm, a, b, c)
    | Art a              -> Art (san a)
    | Name (nm, t)       -> Name (Name.sanitize nm, sanitize san t)
  let rec hash h seed x = match x with
      Skip as c -> Hashtbl.seeded_hash seed c
    | Assign(nm, x,a) as c -> Hashtbl.seeded_hash (Name.hash seed nm) c
    | Seq(c1,c2) -> hash h (hash h seed c1) c2
    | Set (nm, a, b, c) -> Hashtbl.seeded_hash (Name.hash seed nm) x
    | If (b, c1, c2) -> hash h (hash h (Hashtbl.seeded_hash seed b) c1) c2
    | While (nm, b, c) -> hash h (Hashtbl.seeded_hash (Name.hash seed nm) b) c
    | Art a -> h seed a
    | Name (nm, c) -> hash h (Name.hash seed nm) c
end

module Cmd : sig
  include Articulated.Fix(ArtLib)(Name)(CmdP).S
  val name_of : t -> name option
end = struct
  include Articulated.Fix(ArtLib)(Name)(CmdP)
  let name_of : t -> name option = function
    | Skip _ | Seq _ | If _ | Art _ -> None
    | Assign (nm, _, _) | Set (nm, _, _, _) | While (nm, _, _) -> Some nm
end

let vars : Cmd.t -> string list =
  let avars : aexpr -> string list =
    let rec loop (acc : string list) : aexpr -> string list = function
      | Int _ -> acc
      | Plus  (e0, e1) -> loop (loop acc e0) e1
      | Minus (e0, e1) -> loop (loop acc e0) e1
      | Times (e0, e1) -> loop (loop acc e0) e1
      |   Div (e0, e1) -> loop (loop acc e0) e1
      | Alloc a -> loop acc a
      | Get (a0, a1) -> loop (loop acc a0) a1
      | Var x -> x::acc in
    fun a -> List.sort_uniq compare (loop [] a) in
  let bvars : bexpr -> string list =
    let rec loop (acc : string list) : bexpr -> string list = function
      | True | False -> acc
      | Not b -> loop acc b
      | Or (b0, b1) -> loop (loop acc b0) b1
      | And (b0, b1) -> loop (loop acc b0) b1
      | Leq (a0, a1) -> (avars a0) @ (avars a1) @ acc
      | Eq (a0, a1) -> (avars a0) @ (avars a1) @ acc in
    fun a -> List.sort_uniq compare (loop [] a) in
  let vars : Cmd.t -> string list =
    let rec loop (acc : string list) : Cmd.t -> string list = function
      | Skip -> acc
      | Assign (_, x, a) -> x::((avars a)@acc)
      | Seq (e0, e1) -> loop (loop acc e0) e1
      | Set (nm, a, b, c) -> (avars a)@(avars b)@(avars c)@acc
      | If (b, e0, e1)  -> loop (loop (acc@(bvars b)) e0) e1
      | While (_, b, e) -> loop (acc@(bvars b)) e
      | Art a -> loop acc (Cmd.Art.force a)
      | Name (_, e) -> loop acc e in
    fun e -> List.sort_uniq compare (loop [] e) in
  vars

module Eval = struct

  module AB(E : Env.S)(H : Heap.S) = struct

    let rec aeval (nm : Name.t) (r : E.t) (h : H.t) : aexpr -> int * H.t =
      function
      |    Int  n           -> n, h
      |   Plus (e0, e1)     -> let v0, h = aeval nm r h e0 in
                               let v1, h = aeval nm r h e1 in
                               v0 + v1, h
      |  Minus (e0, e1)     -> let v0, h = aeval nm r h e0 in
                               let v1, h = aeval nm r h e1 in
                               v0 - v1, h
      |  Times (e0, e1)     -> let v0, h = aeval nm r h e0 in
                               let v1, h = aeval nm r h e1 in
                               v0 * v1, h
      |    Div (e0, e1)     -> let v0, h = aeval nm r h e0 in
                               let v1, h = aeval nm r h e1 in
                               v0 / v1, h
      |    Var  x           -> (match E.lookup r x with
                                | Some v -> v, h
                                | None   -> failwith ("Unset variable: " ^ x))
      |  Alloc  e           -> let nm', nm'' = Name.fork nm in
                               H.alloc nm' (aeval nm'' r h e)
      |    Get (e0, e1)     -> let v0, h = aeval nm r h e0 in
                               let v1, h = aeval nm r h e1 in
                               (match H.lookup h (v0 + v1) with
                                | Some v ->
                                  v, h
                                | None   -> failwith "Bad addr deref")
        

    let rec beval : Name.t -> E.t -> H.t -> bexpr -> bool * H.t =
      fun nm r h -> function
        | True         -> true, h
        | False        -> false, h
        | Not  b       -> let v, h = beval nm r h b in
                          not v, h
        | And (b0, b1) -> let v0, h = beval nm r h b0 in
                          let v1, h = beval nm r h b1 in
                          v0 && v1, h
        |  Or (b0, b1) -> let v0, h = beval nm r h b0 in
                          let v1, h = beval nm r h b1 in
                          v0 || v1, h
        | Leq (a0, a1) -> let v0, h = aeval nm r h a0 in
                          let v1, h = aeval nm r h a1 in
                          v0 <= v1, h
        |  Eq (a0, a1) -> let v0, h = aeval nm r h a0 in
                          let v1, h = aeval nm r h a1 in
                          v0 = v1, h

  end

  let name_of_int_list nm ints =
    PrimList.fold_left
      (fun nm i ->
         Name.pair (Name.of_string (string_of_int i)) nm)
      nm ints

  module type S = sig
    module   Env :  Env.S
    module  Heap : Heap.S
    module AState : Art.S with type data = Env.t * Heap.t
    val ceval : Name.t -> bool -> int list -> Env.t -> Heap.t -> Cmd.t -> AState.t
  end

  module Standard(Env : Env.S)(Heap : Heap.S) : S = struct

    module Env = Env
    module Heap = Heap
    module State = Types.Tuple2(Env)(Heap)
    module AState = ArtLib.MakeArt(Name)(State)

    include AB(Env)(Heap)

    let ceval =
      let mfn =
        AState.mk_mfn
          (Name.of_string "Implang.CEval#ceval")
          (module Types.Tuple6(Name)(Types.Bool)(IntList)(Env)(Heap)(Cmd))
          (fun mfn (outernm, verbose, coord, r, h, cmd) ->
             let ceval outernm coord r h c =
               mfn.AState.mfn_data (outernm,verbose,coord,r,h,c) in
             let ceval_same_loop = ceval outernm coord in
             let ceval_memo_point outernm coord cmd_nm cmd =
               let nm = (name_of_int_list cmd_nm coord) in
               let art = mfn.mfn_nart nm (outernm,verbose,coord,r,h,cmd) in
               (if verbose then
                  Printf.printf "| ceval | %s || %s | %s | %s | %s | %s \n"
                    (Name.show nm)
                    (Name.show outernm)
                    (IntList.show coord)
                    (Env.show r)
                    (Heap.show h)
                    (Cmd.show cmd)) ;
               AState.force art
             in
             match cmd with
             | Skip -> r, h
             | Assign (assign_nm, x, a) ->
               let nm = name_of_int_list assign_nm coord in
	       let i, h = aeval nm r h a in
               (if verbose then
                  Printf.printf "| Env.ext | %s | (%s,%d) \n%!"
                    (Name.show nm) x i) ;
               Env.ext nm r x i, h

             | Set (nm, e0, e1, e2) ->
               let nm = name_of_int_list nm coord in
               let v0, h = aeval nm r h e0 in
               let v1, h = aeval nm r h e1 in
               let v2, h = aeval nm r h e2 in
               (if verbose then
                  Printf.printf "| Heap.ext | %s | (%d,%d) \n%!"
                    (Name.show nm) (v0 + v1) v2) ;
               r, Heap.ext nm h (v0 + v1) v2

             | Seq (c0, c1) ->
               let r, h = ceval_same_loop r h c0 in
               ceval_same_loop r h c1

             | If (b, c0, c1) ->
               let nm = name_of_int_list outernm coord in
               let v, h = beval nm r h b in
               ceval_same_loop r h (if v then c0 else c1)

             | (While (nm, b, c)) as w ->
               if outernm = nm then (* same loop *)
                 let default_idx::coord_suff = coord in
                 let coord = default_idx+1::coord_suff in
                 ceval_memo_point nm coord nm (If (b, Seq(c, w), Skip))
               else (* entering an inner loop for the first time. *)
                 let coord = 0 :: coord in
                 ceval_memo_point nm coord nm (If (b, Seq(c, w), Skip))

             | Art a ->
               ceval_same_loop r h (Cmd.Art.force a)

             | Name(cmd_nm, cmd) ->
               ceval_memo_point outernm coord cmd_nm cmd
          )
      in
      fun outernm verbose coord cmd r h ->
        mfn.mfn_art (outernm, verbose, coord, cmd, r, h)
      (* else if false then
        mfn.mfn_data (outernm, verbose, coord, cmd, s) *)

  end

  module Balanced(Env : Env.S)(Heap : Heap.S) : S = struct

    module Env = Env
    module Heap = Heap
    module State = Types.Tuple2(Env)(Heap)
    module AState = ArtLib.MakeArt(Name)(State)

    include AB(Env)(Heap)

    module CevalData =
      Types.Sum2
        (State)
        (Types.Tuple6(Name)(Types.Bool)(IntList)(Env)(Heap)(Cmd))
    module CevalArt = ArtLib.MakeArt(Name)(CevalData)

    let ceval_jump : CevalData.t -> CevalData.t =
      let mfn =
        CevalArt.mk_mfn
          (Name.of_string "Implang.Balanced#ceval_jump")
          (module CevalData)
          (fun mfn ((InR (outernm, verbose, coord, r, h, cmd)) as inp) ->
             (*let ceval outernm coord r h c =
               mfn.mfn_data (outernm,verbose,coord,r,h,c) in*)
             let ceval_same_loop r h c = (*ceval outernm coord*) mfn.mfn_data (InR (outernm, verbose, coord, r, h, c)) in
             let ceval_memo_point outernm coord cmd_nm cmd =
               let nm = (name_of_int_list cmd_nm coord) in
               let art = mfn.mfn_nart nm (InR (outernm,verbose,coord,r,h,cmd)) in
               (if verbose then
                  Printf.printf "| ceval | %s || %s | %s | %s | %s | %s \n"
                    (Name.show nm)
                    (Name.show outernm)
                    (IntList.show coord)
                    (Env.show r)
                    (Heap.show h)
                    (Cmd.show cmd)) ;
               CevalArt.force art
               in
             match cmd with
             | Skip -> InL (r, h)

             | Assign (assign_nm, x, a) ->
               let nm = name_of_int_list assign_nm coord in
	       let i, h = aeval nm r h a in
               (if verbose then
                  Printf.printf "| ext | %s | (%s,%d) \n%!"
                    (Name.show nm) x i) ;
               InL (Env.ext nm r x i, h)

             | Set (nm, e0, e1, e2) ->
               let nm = name_of_int_list nm coord in
               let v0, h = aeval nm r h e0 in
               let v1, h = aeval nm r h e1 in
               let v2, h = aeval nm r h e2 in
               InL (r, Heap.ext nm h (v0 + v1) v2)

             | Seq (c0, c1) -> 
	       (match ceval_same_loop r h c0 with
	        | InL (r, h) -> ceval_same_loop r h c1
	        | InR (nm, b, is, r, h, c) ->
                  InR (nm, b, is, r, h, Seq (c, c1)))

             | If (b, c0, c1) ->
               let nm = name_of_int_list outernm coord in
               let v, h = beval nm r h b in
               ceval_same_loop r h (if v then c0 else c1)

             | (While (nm, b, c)) as w ->
               let coord =
                 if outernm = nm
                 then let default_idx::coord_suff = coord in
                      default_idx+1::coord_suff
                 else 0 :: coord
               in
               InR (nm, verbose, coord, r, h, (If (b, Seq(c, w), Skip)))

             | Art a ->
               ceval_same_loop r h (Cmd.Art.force a)

             | Name(cmd_nm, cmd) ->
               ceval_memo_point outernm coord cmd_nm cmd

          )
      in
      mfn.mfn_data

    let ceval : Name.t -> bool -> int list -> Env.t -> Heap.t -> Cmd.t -> AState.t =
      let depth is exp =
        let rec loop acc n =
          if n mod 2 = 1
          then loop (acc+1) (n lsr 1)
          else acc in
        loop 0 (IntList.hash (Cmd.hash 42 (* <-- magic seed *) exp) is)
      in
      let lbound_init = ~-1
      and ubound_init = max_int in
      let loop =
        CevalArt.mk_mfn
          (Name.of_string "Implang.Balanced#loop")
          (module Types.Tuple3(Types.Int)(Types.Int)(CevalData))
          (fun loop (lbound, ubound, cd) -> match cd with
             | InL _ -> cd
             | InR (nm, b, is, r, h, c) ->
               let depth = depth is c in
               if lbound < depth && depth < ubound
               then CevalArt.force (loop.mfn_art (depth, ubound, CevalArt.force (loop.mfn_art (lbound_init, depth, (ceval_jump cd)))))
               else cd)
      in
      let rec driver (cd : CevalData.t) : State.t =
        match cd with
        | InL out -> out
        | InR _ -> driver (CevalArt.force (loop.mfn_art (lbound_init, ubound_init, cd)))
      in
      let state_thunk : Name.t -> CevalData.t -> AState.t =
        let ident =
          AState.mk_mfn (Name.of_string "state_thunk")
            (module CevalData)
            (fun _ c -> driver c)
        in
        fun nm -> ident.mfn_art
      in
      fun nm b is r h c -> (*driver (InR (nm, b, is, r, h, c))*)
        state_thunk nm (InR (nm, b, is, r, h, c))
        

  end

end

end
