(* Preamble. *)
type random_state = Random.State.t
let random_case i rng = Random.State.int rng i
let random_case_30b = Random.State.bits

(* Specialize string as variable. *)
type var = string [@@deriving show]
let random_var rng = String.make 1 (Char.chr (Random.State.int rng 26 + 0x61))

(* Type and random generator. *)
type 'a free_magma =
  | Fm_gen [@weight 3] of 'a
  | Fm_mul [@weight 2] of 'a free_magma * 'a free_magma
  [@@deriving random, show]

(* Test. *)
let () =
  let rng = Random.State.make_self_init () in
  let m = random_free_magma random_var rng in
  print_endline (show_free_magma pp_var m)
