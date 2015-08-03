open Adapton

(* Experiments *)

let  visualize = ref false
let    verbose = ref false
let     record = ref false
let  test_name = ref "all"
let intl_input = ref  8
let fact_input = ref  5
let array_size = ref  8
let eval_strat = ref "balanced" (* { "balanced", "standard" } *)
let    prefill = ref false
let store_type = ref "trie"     (* { "list", "trie" } *)
let  min_depth = ref 4
let     artlib = ref "nominal"
let  sq_matrix = ref 4

let time_string : string =
  let time = string_of_float (Unix.time ()) in
  String.sub time 0 ((String.length time) - 1)
  (*let pad i = let soi = string_of_int i in if i < 10 then "0"^soi else soi in
  let t = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%s-%s-%s_%s.%s.%s"
    (pad (t.tm_year mod 100))
    (pad (t.tm_mon + 1))
    (pad (t.tm_mday + 1))
    (pad t.tm_hour)
    (pad t.tm_min)
    (pad t.tm_sec)*)

module type S = sig val test : unit -> unit end
module Make(ArtLib : ArtLib.S) = struct
  
include Lang.Make(ArtLib)

let rec dfs (acc : Cmd.t -> Name.t option)
  : Cmd.t -> (Name.t * Cmd.Art.t) option = function
  | Name (nm, Art a) ->
    let dat = Cmd.Art.force a in
    match acc dat with
    | Some nm -> Some (nm, a)
    | None -> (match dat with
        | Assign _
        |    Set _
        |   Skip            -> None
        |    Seq (c, c')
        |     If (_, c, c') -> (match dfs acc c with
                                | None -> dfs acc c'
                                | out -> out)
        |  While (_, _, c)  -> dfs acc c)

let get_leftmost_assign_x (x : string) : Cmd.t -> (Name.t * Cmd.Art.t) option =
  dfs (function Assign (nm, y, _) when y = x -> Some nm | _ -> None)

let get_set (x : string) (off : int) (n : int) : Cmd.t -> (Name.t * Cmd.Art.t) option =
  dfs (function
      | Set (nm, Var y, Int o, Int m) when x = y && o = off && m = n -> Some nm
      | _ -> None)

let get_n_leftmost_while (n : int) : Cmd.t -> (Name.t * Cmd.Art.t) option =
  let counter = ref 1 in
  dfs 
    (function
    | While (nm, _, _) when !counter < n -> counter := !counter+1 ; None
    | While (nm, _, _) -> Some nm
    | _ -> None)

let set_set
    (x : string)
    (off : int)
    (n : int)
    (off' : int)
    (n' : int)
    (cmd : Cmd.t) : unit =
  match get_set x off n cmd with
  | None -> failwith "didn't find the right set"
  | Some (nm, a) -> Cmd.Art.set a (Set (nm, Var x, Int off', Int n'))

let set_leftmost_assign_x (x : string) (assign' : Name.t -> Cmd.t) (cmd : Cmd.t) : unit =
  match get_leftmost_assign_x x cmd with
  | None -> failwith ("The var "^x^" does not have a valid assignment in "^(Cmd.show cmd))
  | Some (nm, a) -> Cmd.Art.set a (assign' nm)

let rec deannotate : Cmd.t -> cmd = function
  | Skip -> Skip
  | Assign (_, x, a) -> Assign (x, a)
  | Set (_, a, b, c) -> Set (a, b, c)
  | Seq (c1, c2) -> Seq(deannotate c1,deannotate c2)
  | If (b, c1, c2) -> If (b, deannotate c1, deannotate c2)
  | While (_, b, c) -> While (b, deannotate c)
  | Name (_, t) -> deannotate t
  | Art a -> deannotate (Cmd.Art.force a)

let rec annotate : cmd -> Cmd.t =
  fun c ->
  let recur (c:cmd) : Cmd.t = match c with
    | Skip -> Skip
    | Assign (x, a) -> Assign (Name.nondet (), x, a)
    | Set (a, b, c) -> Set (Name.nondet (), a, b, c)
    | Seq (c1, c2) ->
       Seq (annotate c1, annotate c2)
    | If (b, c1, c2) ->
       If (b, annotate c1, annotate c2)
    | While (b, c) ->
       While (Name.nondet (), b, annotate c)
    (*| AWhile (a, b, c) ->
       AWhile ((Name.nondet (), a), b, annotate ~verbose c)*)
  in
  let nm = Name.nondet () in
  let nm1, nm2 = Name.fork nm in
  let r = recur c in
  let cell = Cmd.Art.cell nm1 r in
  let cmd = CmdGen.Name (nm2, CmdGen.Art cell) in
  (if !verbose then
      Printf.printf "| %s | %s | %s | \n" (Name.show nm) (Cmd.show cmd) (Cmd.show r)) ;
  cmd

let rec au : Cmd.t -> (string * string) list * Cmd.t =
  let mk_gs_var () : unit -> string =
    let n = ref 0 in
    (fun () -> let n_0 = !n in n := n_0 + 1; (string_of_int n_0))
  in
  let gs_var : unit -> string = mk_gs_var ()
  in
  let rec au_aexpr (m : (string * string) list) (a:aexpr) : aexpr = match a with
    | Int _ -> a
    |  Plus (a1, a2) ->  Plus (au_aexpr m a1, au_aexpr m a2)
    | Minus (a1, a2) -> Minus (au_aexpr m a1, au_aexpr m a2)
    | Times (a1, a2) -> Times (au_aexpr m a1, au_aexpr m a2)
    |   Div (a1, a2) ->   Div (au_aexpr m a1, au_aexpr m a2)
    | Var x when List.mem_assoc x m -> Var (List.assoc x m)
    | Var x -> failwith ("All Vars must be initialized with Assign: "^x)
    | Alloc a -> Alloc (au_aexpr m a)
    | Get (a0, a1) -> Get (au_aexpr m a0, au_aexpr m a1)
  in 
  let rec au_bexpr (m : (string * string) list) (b:bexpr) : bexpr = match b with
    | True | False -> b
    | Not  b1      -> Not (au_bexpr m b1)
    | And (b1, b2) -> And (au_bexpr m b1, au_bexpr m b2)
    |  Or (b1, b2) ->  Or (au_bexpr m b1, au_bexpr m b2)
    |  Eq (a1, a2) ->  Eq (au_aexpr m a1, au_aexpr m a2)
    | Leq (a1, a2) -> Leq (au_aexpr m a1, au_aexpr m a2)
  in
  let rec loop (m : (string * string) list) (c:Cmd.t) : (string * string) list * Cmd.t =
    match c with
    | Skip -> (m, Skip)
    | Set (nm, a0, a1, a2) -> m, Set (nm, au_aexpr m a0, au_aexpr m a1, au_aexpr m a2)
    | Assign (nm, x, a) ->
      (match List.assoc x m with
      | x' -> (m, Assign (nm, x', au_aexpr m a))
      | exception Not_found ->
        let x' = gs_var ()  in
        let m' = (x, x')::m in
        (m', Assign (nm, x', au_aexpr m' a)))
    | Seq (c1, c2) ->
      let m',  c1' = loop m c1 in
      let m'', c2' = loop m' c2 in
      (List.append m' m'', Seq (c1', c2'))
    | If (b, c1, c2) ->
      let m',  c1' = loop m c1 in
      let m'', c2' = loop m' c2 in
      let m''' = List.append m' m'' in
      (m''', If (au_bexpr m''' b, c1', c2'))
    | While (nm, b, c) ->
      let m', c' = loop m c in
      (m', While (nm, au_bexpr m' b, c'))
    | Name (nm, Art a) ->
      let m', c' = loop m (Cmd.Art.force a) in
      Cmd.Art.set a c';
      (m', Name (nm, Art a))
  in loop []

(*
  | Art of 'a
  | Name of Name.t * 'a art_cmd
 *)

(*
let main =
  let p = annotate fact in
  let s = ceval `Nil p in
  print_string (List.Data.show s);
  replace_leftmost p (Assign ("x", Int 6));
  let s1 = ceval `Nil p in
  print_string (List.Data.show s1)
 *)

let stats_string msg stats =
  Printf.sprintf
    "%s: dirty:%d, clean:%d, create: %d, evaluate: %d\n%!"
    msg
    stats.Statistics.dirty
    stats.Statistics.clean
    stats.Statistics.create
    stats.Statistics.evaluate

module type EXP = sig
  module Env : Env.S
  module Heap : Heap.S
  val test_cmd_mutation : Cmd.t -> (unit -> Env.t) -> (unit -> Heap.t) -> (Cmd.t -> unit) -> (string * Statistics.t * string * Statistics.t)
end

module Experiments(Eval : Eval.S) : EXP = struct

  include Eval

  let sto_string msg (r, h) =
    Printf.sprintf "Sto %s:\n------------\n```\n%s\n%s\n```\n"
      msg (Env.show r) (Heap.show h)

  let do_run msg f =
    let (_, stats) as out =
      Statistics.measure (
        fun () ->
          (if !verbose then begin
            Printf.printf "\n\n Run %s\n---------------------------\n\n" msg ;
            Printf.printf "| mfn | name | ext  | outernm  | coord | Sto | Cmd |\n" ;
            Printf.printf "|-----|------|---|---|---|---|---|\n"
          end) ;
          f ()
      )
    in
    (if !verbose then begin
      Printf.printf "%s\n%!"
        (stats_string (Printf.sprintf "**Run %s**" msg) stats)
        (*(sto_string msg s)*)
      end) ;
    out

  let test_cmd_mutation (cmd : Cmd.t) r0 h0 (mutator : Cmd.t -> unit) =
    let root_loop = Name.nondet () in
    (*let out = open_out "results/viz_out" in*)
    (if !verbose then
        (print_endline "cmd:";
         print_endline (string_of_cmd (deannotate cmd)))) ;
    (if !verbose then print_endline "$$$ Before run 1:") ;
    let ((s, (r, h)), stats1) =
      do_run (string_of_int 1)
        (fun () ->
          let s = ceval root_loop !verbose [] (r0 ()) (h0 ()) cmd in
          let r, h = AState.force s in s, (r, h))
    in
    (*(if !visualize then Viz.flush_ticks_out out) ;*)
    let h_str = (Env.show (Env.force r)) ^ "\n" ^ (Heap.show (Heap.force h)) in
    (if !verbose then print_endline ("$$$ After run 1:\n"^h_str)) ;
    let ((s, (newr, newh)), stats2) =
      do_run (string_of_int 2)
        (fun () -> mutator cmd ; (*(if !visualize then Viz.flush_ticks_out out) ;*)
          let newr, newh = AState.force s in
          (*let newr, newh = State.force (ceval root_loop !verbose [] (r0 ()) (h0 ()) cmd)
            in*)
          s, (Env.force newr, Heap.force newh))
    in
    (*(if !visualize then Viz.flush_ticks_out out) ;*)
    (if !verbose then
        (print_endline "cmd':";
         print_endline (string_of_cmd (deannotate cmd)))) ;
    (*close_out out;*)
    let h_str' = (Env.show newr) ^ "\n" ^ (Heap.show newh) in
    (if !verbose then print_endline ("$$$ After run 2:\n"^h_str')) ;
    (h_str, stats1, h_str', stats2)

end

let write_file (content : string) (path : string) : bool =
  let chan = open_out_gen [Open_append; Open_creat; Open_binary] 0o644 path in
  let out =
    try output_string chan content; flush chan; true
    with exc -> print_endline (Printexc.to_string exc); false in
  close_out chan;
  if not out then
    (Printf.printf "Failed to write %s.\n" path; false)
  else if out && Sys.file_exists path && !verbose then
    (Printf.printf "Successfully wrote %s.\n" path; true)
  else false

let remove_spaces = Str.global_replace (Str.regexp_string " ") "-"

let mut_test
    (cmd      : cmd)
    (test     : string)
    (mutator  : Cmd.t -> unit) : unit =
  (if !verbose then begin
    Printf.printf "Articulated Program Test: %s\n-------------\n\n" test ;
    Printf.printf "Trie min_depth: %i\n-------------\n\n" !min_depth ;
    Printf.printf "| name | Name, Art | Cmd |\n" ;
    Printf.printf "|------|-----------|----------|\n"
  end) ;
  let module Env = (val
      if !store_type = "trie"
      then (module Env.NTrie : Env.S)
      else (module Env.Assoc : Env.S))
  in
  let module Heap = (val
      if !store_type = "trie"
      then (module Heap.NTrie : Heap.S)
      else (module Heap.Assoc : Heap.S))
  in
  let module Exp = (val
      if !eval_strat = "balanced"
      then (module Experiments(Eval.Balanced(Env)(Heap)) : EXP)
      else (module Experiments(Eval.Standard(Env)(Heap)) : EXP))
  in
  let cmd' = annotate cmd in
  let r0 =
    if !prefill
    then (fun () -> Exp.Env.prefill ~min_depth:!min_depth (vars cmd'))
    else (fun () -> Exp.Env.mt ~min_depth:!min_depth) in
  let h0 = (fun () -> Exp.Heap.mt ~min_depth:!min_depth) in
  let h_str, stats1, h_str', stats2 =
    Exp.test_cmd_mutation
      cmd'
      r0
      h0
      mutator
  in
  (if !record then
      (* Writes the stats to a CSV file *)
      let csv_line =
        Printf.sprintf "%s,\"%s\",\"%s\",\"%s%s\",%i,%f,%i,%i,%i,%i,%f,%i,%i,%i,%i"
          time_string
          !artlib
          !eval_strat
          (if !prefill then "Prefilled " else "")
          !store_type
          !min_depth
          stats1.Statistics.time
          stats1.Statistics.dirty
          stats1.Statistics.clean
          stats1.Statistics.create
          stats1.Statistics.evaluate
          stats2.Statistics.time
          stats2.Statistics.dirty
          stats2.Statistics.clean
          stats2.Statistics.create
          stats2.Statistics.evaluate
      in
      let cols =
        "test_ran_at,artlib,eval_strat,store_type,min_depth,time1," ^
          "dirty1,clean1,create1,eval1,time2,dirty2,clean2,create2,eval2"
      in
      let dir = "results" in
      (if not (Sys.file_exists dir) then Unix.mkdir dir 0o755) ;
      let file_name = Printf.sprintf "results/%s.csv" (remove_spaces test) in
      let content =
        if Sys.file_exists file_name
        then Printf.sprintf "%s\n"     csv_line
        else Printf.sprintf "%s\n%s\n" cols csv_line in
      ignore(write_file content file_name)) ;
  ()



let rec seq : cmd list -> cmd = fun cs ->
  match cs with
  | [] -> Skip
  | c::cs -> Seq (c, seq cs)

let nat_up_exclusive f n b =
  let rec loop acc m =
    if m >= n
    then acc
    else loop (f m acc) (m + 1)
  in
  loop b 0

let gs =
  let i = ref 0 in
  (fun () -> let i0 = !i in i := i0+1 ; "_" ^ (string_of_int i0))


(*
  Don't forget: dimensions for matrices are given rows x cols for some
  terrible reason.

  [| [| 1,2 |], [| 1,2 |], [| 1,2 |] |] [| [| 1,2,3 |], [| 1,2,3 |] |]

    ==
  
  1 2               3 6 9
  1 2  x  1 2 3  =  3 6 9
  1 2     1 2 3     3 6 9

 a is n x m
 b is m x p
let mult a b =
  let n = Array.length a
  and m = Array.length a.(0) (* also len(b) *)
  and p = Array.length b.(0) in
  let c = Array.make n [||]
  and x = ref 0 in
  while !x <= p-1 do
    c.(!x) <- Array.make n 0 ;
    x := !x + 1
  done ;
  let i = ref 0 in
  while !i <= n-1 do
    let j = ref 0 in
    while !j <= p-1 do
      let s = ref 0 in
      let k = ref 0 in
      while !k <= m-1 do
        s := !s + (a.(!i).(!k) * b.(!k).(!j)) ;
        k := !k + 1
      done ;
      c.(!i).(!j) <- !s ;
      j := !j + 1
    done ;
    i := !i + 1
  done ;
  c

*)
let matrix_mult_gen : int -> int -> int -> (int * int -> int) -> (int * int -> int) -> cmd =
  (* returns the gensymed variable init for the loop counter and the while loop *)
  let matrix : int -> int -> string -> (int * int -> int) -> cmd * cmd =
    (fun n m name f ->
      let tmp = gs () in
      let loop_var : cmd = Assign (tmp, Int 0)
      in
      let alloc : cmd = While (Leq (Var tmp, Minus (Int n, Int 1)),
                               seq [ Set (Var name, Var tmp, Alloc (Int m))
                                   ; Assign (tmp, Plus (Var tmp, Int 1))
                                   ])
      in
      let init : cmd =
        nat_up_exclusive
          (fun i ->
            nat_up_exclusive
              (fun j acc -> (Seq (Set (Get (Var name, Int i), Int j, Int (f (i,j))), acc) : cmd))
              m)
          n
          Skip
      in
      (loop_var, Seq (alloc, init)))
  in
  fun n m p af bf ->
    let nx = "n"
    and mx = "m"
    and px = "p"
    and ax = "a"
    and bx = "b"
    and cx = "c"
    and ix = "i"
    and jx = "j"
    and kx = "k"
    and sx = "s"
    in
    let cf = fun _ -> 0
    in
    let lva, init_mtx_a = matrix n m ax af
    and lvb, init_mtx_b = matrix m p bx bf
    and lvc, init_mtx_c = matrix n p cx cf
    in
    seq [ Assign (nx, Int n)
        ; Assign (mx, Int m)
        ; Assign (px, Int p)
        ; Assign (ax, Alloc (Int n))
        ; Assign (bx, Alloc (Int m))
        ; Assign (cx, Alloc (Int p))
        ; lva        ; lvb        ; lvc
        ; init_mtx_a ; init_mtx_b ; init_mtx_c
        ; Assign (ix, Int 0)
        ; While (Leq (Var ix, Minus (Var nx, Int 1)),
      seq [ Assign (jx, Int 0)
          ; While (Leq (Var jx, Minus (Var px, Int 1)),
        seq [ Assign (sx, Int 0)
            ; Assign (kx, Int 0)
            ; While (Leq (Var kx, Minus (Var mx, Int 1)),
                Seq (Assign (sx, Plus (Times (Get (Get (Var ax, Var ix), Var kx),
                                              Get (Get (Var bx, Var kx), Var jx)),
                                       Var sx)),
                     Assign (kx, Plus (Var kx, Int 1))))
            ; Set (Get (Var cx, Var ix), Var jx, Var sx)
            ; Assign (jx, Plus (Var jx, Int 1))
            ])
          ; Assign (ix, Plus (Var ix, Int 1))
        ])
        ]

let matrix_mult_ext : int -> int -> int -> cmd =
  (* returns the gensymed variable init for the loop counter and the while loop *)
  let matrix : string -> string -> string -> cmd * cmd =
    (fun nx mx name ->
       let tmp1 = gs () in
       let loop_var1 : cmd = Assign (tmp1, Int 0)
       in
       let alloc : cmd = While (Leq (Var tmp1, Minus (Var nx, Int 1)),
                                seq [ Set (Var name, Var tmp1, Alloc (Var mx))
                                    ; Assign (tmp1, Plus (Var tmp1, Int 1))
                                    ])
       in
       let tmp2 = gs ()
       and tmp3 = gs ()
       in
       let init : cmd =
         Seq (Assign (tmp2, Int 0),
              While (Leq (Var tmp2, Minus (Var nx, Int 1)),
                     seq [ Set (Var name, Var tmp2, Alloc (Var mx))
                         ; Assign (tmp3, Int 0)
                         ; While (Leq (Var tmp3, Minus (Var mx, Int 1)),
                                  Seq (Set (Get (Var name, Var tmp2), Var tmp3, Int 10),
                                       Assign (tmp3, Plus (Var tmp3, Int 1))))
                         ; Assign (tmp2, Plus (Var tmp2, Int 1))
                         ]))
       in
       (loop_var1, Seq (alloc, init)))
  in
  fun n m p ->
    let nx = "n"
    and mx = "m"
    and px = "p"
    and ax = "a"
    and bx = "b"
    and cx = "c"
    and ix = "i"
    and jx = "j"
    and kx = "k"
    and sx = "s"
    in
    let cf = fun _ -> 0
    in
    let lva, init_mtx_a = matrix nx mx ax
    and lvb, init_mtx_b = matrix mx px bx
    and lvc, init_mtx_c = matrix nx px cx
    in
    seq [ Assign (nx, Int n)
        ; Assign (mx, Int m)
        ; Assign (px, Int p)
        ; Assign (ax, Alloc (Var nx))
        ; Assign (bx, Alloc (Var mx))
        ; Assign (cx, Alloc (Var px))
        ; lva        ; lvb        ; lvc
        ; init_mtx_a ; init_mtx_b ; init_mtx_c
        ; Assign (ix, Int 0)
        ; While (Leq (Var ix, Minus (Var nx, Int 1)),
      seq [ Assign (jx, Int 0)
          ; While (Leq (Var jx, Minus (Var px, Int 1)),
        seq [ Assign (sx, Int 0)
            ; Assign (kx, Int 0)
            ; While (Leq (Var kx, Minus (Var mx, Int 1)),
                Seq (Assign (sx, Plus (Times (Get (Get (Var ax, Var ix), Var kx),
                                              Get (Get (Var bx, Var kx), Var jx)),
                                       Var sx)),
                     Assign (kx, Plus (Var kx, Int 1))))
            ; Set (Get (Var cx, Var ix), Var jx, Var sx)
            ; Assign (jx, Plus (Var jx, Int 1))
            ])
          ; Assign (ix, Plus (Var ix, Int 1))
        ])
        ]


(*

Places the maximum value of ARR in MAX.

LEN = 8
ARR = alloc(LEN)
ARR[4] = 40 ;
ARR[7] = 42 ;
ARR[5] = 41 ;
MAX := 0 ;
while (not (LEN <= 1)) {
  I := 0 ;
  while (I <= LEN - 2) {
    M := 0 ; 
    if (ARR[I + 1] <= ARR[I]) {
      M := ARR[I]
    } else {
      M := ARR[I + 1]
    } ;
    ARR[I / 2] = M
    I = I + 2
  } ;
  LEN := LEN / 2
}
MAX := ARR[0]


Original:

int MAX;
void array max(int* arr, int len) {
  while(len > 1) {
    for(int i = 0; i < len - 1; i += 2) {
      int m;
      max(arr[i], arr[i + 1], &m);
      arr[i / 2] = m;
    }
    len = len / 2;
  }
  MAX = arr[0];
}


  len := !len ;
  arr := alloc(len) ;
  random_sets ;
  explicit_sets ;
  max := 0 ;
  while (not (len <= 1)) {
    i := 1 ;
    while (i <= len - 2) {
      m := 0 ;
      if (arr[i+1] <= arr[i]) {
        m = arr[i]
      } else {
        m = arr[i+1]
      }
      i := i+2
    }
    len := len / 2
  }
  max := arr[0]

*)
let random_array_max
  (len : int)
  (ran_max : int)
  (explicit_elts : (int * int) list)
  : cmd =
  Random.self_init () ;
  let random_sets : cmd =
    nat_up_exclusive
      (fun n acc -> (Seq (Set (Var "arr", Int n, Int (Random.int ran_max)), acc) : cmd))
      len
      Skip
  in
  let explicit_sets : cmd =
    List.fold_right
      (fun (i, v) acc -> (Seq (Set (Var "arr", Int i, Int v), acc) : cmd))
      explicit_elts
      Skip
  in

  seq [ Assign ("len", Int len)
      ; Assign ("arr", Alloc (Var "len"))
      ; random_sets
      ; explicit_sets
      ; Set (Var "arr", Int 0, Int 40)
      ; Assign ("max", Int 0)
      ; While (Not (Leq (Var "len", Int 0)),
               seq [ Assign ("i", Int 0)
                   ; While (Leq (Var "i", Minus (Var "len", Int 1)),
                            seq [ Assign ("m", Int 0)
                                ; If (Leq (Get (Var "arr", Plus (Var "i", Int 1)), Get (Var "arr", Var "i")),
                                      Assign ("m", Get (Var "arr", Var "i")),
                                      Assign ("m", Get (Var "arr", Plus (Var "i", Int 1))))
                                ; Set (Var "arr", Div (Var "i", Int 2), Var "m")
                                ; Assign ("i", Plus (Var "i", Int 2))
                                ])
                   ; Assign ("len", Div (Var "len", Int 2))
                   ])
      ; Assign ("max", Get (Var "arr", Int 0))
      ]

let arr_max : cmd = random_array_max !array_size 1 [1, 42] (*[ 7, 42 ; 5, 41 ]*)


let random_array_ext
  (len : int)
  (ran_max : int)
  (explicit_elts : (int * int) list)
  : cmd =
  Random.self_init () ;
  let tmp1 = gs () in
  let random_sets : cmd =
    Seq (Assign (tmp1, Int 0),
         While (Leq (Var tmp1, Minus (Var "len", Int 1)),
                Seq (Set (Var "arr", Var tmp1, Int (Random.int ran_max)),
                     Assign (tmp1, Plus (Var tmp1, Int 1)))))
  in
  let explicit_sets : cmd =
    List.fold_right
      (fun (i, v) acc -> (Seq (Set (Var "arr", Int i, Int v), acc) : cmd))
      explicit_elts
      Skip
  in
  seq [ Assign ("len", Int len)
      ; Assign ("arr", Alloc (Var "len"))
      ; random_sets
      ; explicit_sets
      ; Set (Var "arr", Int 0, Int 40)
      ; Assign ("max", Int 0)
      ; While (Not (Leq (Var "len", Int 0)),
               seq [ Assign ("i", Int 0)
                   ; While (Leq (Var "i", Minus (Var "len", Int 1)),
                            seq [ Assign ("m", Int 0)
                                ; If (Leq (Get (Var "arr", Plus (Var "i", Int 1)), Get (Var "arr", Var "i")),
                                      Assign ("m", Get (Var "arr", Var "i")),
                                      Assign ("m", Get (Var "arr", Plus (Var "i", Int 1))))
                                ; Set (Var "arr", Div (Var "i", Int 2), Var "m")
                                ; Assign ("i", Plus (Var "i", Int 2))
                                ])
                   ; Assign ("len", Div (Var "len", Int 2))
                   ])
      ; Assign ("max", Get (Var "arr", Int 0))
      ]

let arr_max : cmd = random_array_max !array_size 1 [1, 42]
let arr_ext : cmd = random_array_ext !array_size 1 []

(*
let int_log q =
  let r = ref 1 in
  let s = ref 0 in
  while !r <= q do
    s := !s + 1;
    r := !r * 2;
  done;
  s := !s - 1;
  !s
*)

(* places the int log_2 of q in s *)
let int_log : cmd =
  seq [ Assign ("zz", Int 0)
      ; Assign ("q", Int !intl_input)
      ; Assign ("r", Int 1)
      ; Assign ("s", Int 0)
      ; While (Leq (Times (Var "r", Int 2), Var "q"),
               seq [ Assign ("s",  Plus (Var "s", Int 1))
                   ; Assign ("r", Times (Var "r", Int 2))
                   ])
      ; Assign ("c", Int 42)
      ; Assign ("d", Int 42)
      ]

let fact : cmd =
  seq [ Assign ("z", Int 0)
      ; Assign ("n", Int !fact_input)
      ; Assign ("f", Int 1)
      ; While (Leq (Int 1, Var "n"),
	       seq [Assign ("f", Times (Var "n", Var "f"));
	            Assign ("n", Minus (Var "n", Int 1))])
      ; Assign ("a", Int 42)
      ; Assign ("b", Int 43)
      ]

let prog_seq : cmd = Seq (int_log, fact)

let replace_init_val name prog x i' =
  let test = name in
  mut_test
    prog
    test
    (set_leftmost_assign_x x (fun nm -> Assign (nm, x, Int i')))

let replace_init_vals name prog xis =
  let test = name in
  mut_test
    prog
    test
    (fun cmd ->
       let rec loop = function
         | [] -> ()
         | (x, i)::xis -> set_leftmost_assign_x x (fun nm -> Assign (nm, x, Int i)) cmd ;
                          loop xis
       in
       loop xis)

let swap_assignments name prog x y =
  let test = name ^ " swap assign" in
  mut_test
    prog
    test
    (fun cmd ->
      let Some (_, zass) = get_leftmost_assign_x x cmd in
      let Some (_, nass) = get_leftmost_assign_x y cmd in
      let zdat = Cmd.Art.force zass in
      let ndat = Cmd.Art.force nass in
      Cmd.Art.set zass ndat ; Cmd.Art.set nass zdat)

let swap_while name prog n m =
  let test = name ^ " swap while order" in
  mut_test
    prog
    test
    (fun cmd ->
      let Some (_, w1) = get_n_leftmost_while n cmd in
      let Some (_, w2) = get_n_leftmost_while m cmd in
      let w1d = Cmd.Art.force w1 in
      let w2d = Cmd.Art.force w2 in
      Cmd.Art.set w1 w2d ; Cmd.Art.set w2 w1d)

let swap_subprog () =
  mut_test
    prog_seq
    "swap subprograms"
    (function Name (nm, Art a) -> match Cmd.Art.force a with
                                    | Seq (fact, int_log) -> Cmd.Art.set a (Seq (int_log, fact)))

let arr_repl numstr newloc newval =
  mut_test
    arr_max
    ("array max repl"^numstr)
    (set_set "arr" 0 40 newloc newval) (* changes answer, not arr loc *)
    (* (set_set "arr" 4 40 3 43) changes answer, changes arr loc *)

   

let matrix_mult_assign () =
  let gen = function _,j -> j+1 in
  swap_assignments "matrix mult" (matrix_mult_gen !sq_matrix !sq_matrix !sq_matrix gen gen) "a" "b"

let matrix_mult_while () =
  let gen = function _,j -> j+1 in
  swap_while "matrix mult" (matrix_mult_gen !sq_matrix !sq_matrix !sq_matrix gen gen) 1 2

let matrix_ext () =
  replace_init_vals "matrix mult ext"
    (matrix_mult_ext !sq_matrix !sq_matrix !sq_matrix)
    [ "n", !sq_matrix + 5
    ; "m", !sq_matrix + 5
    ; "p", !sq_matrix + 5
    ]
  

(*let int_log_tests () =
  replace_init_val "int log mutate unused"     int_log "zz" 6  ;
  swap_assignments "int log begin swap assign" int_log "r" "s" ;
  swap_assignments "int log end swap assign"   int_log "c" "d" ;
  ()
  | "int_log" -> int_log_tests ()*)

let test () = match !test_name with
  | "fact1"   -> replace_init_val "fact mutate unused init" fact "z" 6
  | "fact2"   -> replace_init_val "fact mutate iter init"   fact "n" (!fact_input + 1)
  | "fact3"   -> swap_assignments "fact begin" fact "n" "f"
  | "fact4"   -> swap_assignments "fact end"   fact "a" "b"
  | "array1"  -> arr_repl "1" 0 43
  | "array2"  -> arr_repl "2" 7 43
  | "array3"  -> arr_repl "3" (!array_size - 1) 43
  | "array4"  -> replace_init_val "arr ext" arr_ext "len" (!array_size * 2)
  | "swap"    -> swap_subprog ()
  | "matrix1" -> matrix_mult_assign ()
  | "matrix2" -> matrix_mult_while ()
  | "matrix3" -> matrix_ext ()

end

let main () =
  let this_bin_name () =
    let name = Sys.executable_name in
    let len  = String.length name in
    let ind  = String.rindex_from name (len-1) '/' in
    String.sub name (ind+1) (len-ind-1)
  in
  let name = this_bin_name () in
  Arg.parse
    [ "--verbose",  Arg.Set verbose,  "Lots of naming information to STDOUT"
    ; "--record", Arg.Set record, "Write results to CSV files"
    ; "--visualize", Arg.Set visualize, "Write visualization output"
    ; "--int-log", Arg.Set_int intl_input, "log_2 n (takes a nat as an argument)"
    ; "--fact", Arg.Set_int fact_input, "fact n (takes a nat as an argument)"
    ; "--array-size",
      Arg.Int
        (fun n ->
          if n land (-n) = n
          then array_size := n
          else failwith ("better use an array-size that is a power of two before you " ^ 
                            "search for a bug for 10 hours while questioning your sanity")),
      "size of randomly initialized array"
    ; "--test", Arg.Set_string test_name, "{ fact1, fact2, fact3, fact4, swap, array1, array2, array3, matrix1, matrix2 }"
    ; "--artlib", Arg.Set_string artlib, "{ nominal, structural, fromscratch }"
    ; "--eval", Arg.Set_string eval_strat, "{ standard, balanced }"
    ; "--prefill", Arg.Set prefill, "prefilled store or not"
    ; "--store", Arg.Set_string store_type, "{ trie, list }"
    ; "--min-depth", Arg.Set_int min_depth, "{ 1..30 }"
    ; "--matrix-dim", Arg.Set_int sq_matrix, "any positive, used for the sides of square matrices"
    ]
    (fun s -> print_endline ("Unexpected anonymous argument passed to "^name^": "^s; exit 1))
    ("Usage: "^name^" {--verbose, --record, --visualize}") ;
  let module Experiments = (val match !artlib with
    | "structural"   -> (module Make(Insts.Structural) : S)
    | "fromscratch"  -> (module Make(Insts.FromScratch) : S)
    | "nominal"      -> (module Make(Insts.Nominal) : S)
    | "sac"          -> (module Make(Alternatives.Sac.ArtLib) : S)
    )
  in
  print_endline
    (Printf.sprintf "Testing %s, artlib=%s, eval=%s, store=%s%s, md=%i"
       !test_name
       !artlib
       !eval_strat
       (if !prefill then "prefilled " else "")
       !store_type
       !min_depth) ;
  try Experiments.test () ; exit 0
  with exc ->
    print_endline "Exception occurred!" ;
    print_endline (Printexc.to_string exc) ;
    exit 1

let _ = main ()
