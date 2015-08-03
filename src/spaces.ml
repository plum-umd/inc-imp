open Adapton

module Make(ArtLib : ArtLib.S) = struct

(* Maps for use as envs and stos *)
module Map = struct

  module type S = sig
    type k
    type v
    include Articulated.S with type name = Name.t
    val mt : ?min_depth:int -> t
    val lookup : t -> k -> v option
    val ext : Name.t -> t -> k -> v -> t
    val force : t -> t
  end

  module NTrie
      (K : Data.S)
      (V : Data.S)
    : S with type k = K.t
         and type v = V.t = struct

    include Trie.Map.Make(Name)(ArtLib)(K)(V)

    let mt ?(min_depth = 1) = empty ~min_depth
    let lookup = find
    let ext = nadd

  end

  module Assoc
      (K : Data.S)
      (V : Data.S)
    : S with type k = K.t
         and type v = V.t = struct
    type k = K.t
    type v = V.t

    module St = SpreadTree.Make(ArtLib)(Name)(Types.Tuple2(K)(V))
    include St.List

    let mt ?(min_depth = 0) : t = `Nil

    let rec lookup : t -> 'a -> 'b option =
      fun s x -> match s with
          `Cons ((y, b), s) when (y = x) -> Some b
        | `Cons (_, s) -> lookup s x
        | `Nil -> None
        | `Name(_, s) -> lookup s x
        | `Art(a) -> lookup (Art.force a) x

    let ext : Name.t -> t -> k -> v -> t =
      let list_mfn =
        Art.mk_mfn (Name.gensym "sto") (module St.List) (fun _ l -> l)
      in
      fun nm s x v ->
        let nm1, nm2 = Name.fork nm in
        `Name(nm1, `Art( list_mfn.mfn_nart nm2 (`Cons ((x, v), s))))

    let rec force : t -> t = function
      | `Cons (x, s) -> `Cons (x, force s)
      | `Nil -> `Nil
      | `Name (_, s) -> force s
      | `Art a -> force (Art.force a)
  end

end

module Env : sig
  module type S = sig
    include Map.S with type k = string
                   and type v = int
    val prefill : ?min_depth:int -> k list -> t
  end
  module NTrie : S
  module Assoc : S
end = struct

  module type S = sig
    include Map.S with type k = string
                   and type v = int
    val prefill : ?min_depth:int -> k list -> t
  end

  module Make (M : Map.S with type k = string
                          and type v = int) = struct

    include M

    let prefill ?(min_depth = 1) : k list -> t =
      List.fold_left
        (fun r x ->
           let nm = (*Name.gensym x*) Name.nondet () in
           ext nm r x min_int)
        (mt ~min_depth)

  end

  module NTrie = Make(Map.NTrie(Types.String)(Types.Int))
  module Assoc = Make(Map.Assoc(Types.String)(Types.Int))

end

module Heap : sig
  module type S = sig
    include Map.S with type k = int
                   and type v = int
    val alloc : Name.t -> (int * t) -> (int * t)
  end
  module NTrie : S
  module Assoc : S
end = struct

  module type S = sig
    include Map.S with type k = int
                   and type v = int
    val alloc : Name.t -> (int * t) -> (int * t)
  end

  module Make(M : Map.S with type k = int
                         and type v = int) = struct

    include M

    let alloc : Name.t -> (int * t) -> (int * t) =
      let gensym : unit -> int =
        let n = ref 0 in
        (fun () -> let n0 = !n in n := n0+1 ; n0)
      in
      fun nm (n, s) ->
        let a0 = gensym () in
        let rec init (nm : Name.t) (acc : t) : int -> t =
          fun m ->
            if m = n then acc
            else
              let  a' = gensym () in
              let nm', nm'' = Name.fork nm in
              init nm' (ext nm'' acc a' 0) (m+1)
        in
        let nm', nm'' = Name.fork nm in
        (a0, init nm' (ext nm'' s a0 0) 1)

  end

  module NTrie = Make(Map.NTrie(Types.Int)(Types.Int))
  module Assoc = Make(Map.Assoc(Types.Int)(Types.Int))

end

end
