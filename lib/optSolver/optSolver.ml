(*
 * Herbert Kociemba's RubiksCube-OptimalSolver
 * modified - now in ocaml (the tables are still generated using python)
 * (C) 2024 by lumin0u (lumin00u@gmail.com)
 * (C) 2021 by Herbert Kociemba (kociemba@t-online.de)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *)

open Algorithm
open AlgEvaluatorElem
open SMoves
open SPruning
open SSymmetries
open CuberProfile

type maneuver = CuberProfile.maneuver

let ordinal_e_move (m: e_move): int =
  match m with
  | U -> 0 | R -> 1 | F -> 2 | D -> 3 | L -> 4 | B -> 5

type coord_cube = {
  _UD_flip: int;
  _RL_flip: int;
  _FB_flip: int;
  _UD_twist: int;
  _RL_twist: int;
  _FB_twist: int;
  _UD_slice_sorted: int;
  _RL_slice_sorted: int;
  _FB_slice_sorted: int;
  corners: int;
  _UD_dist: int;
  _RL_dist: int;
  _FB_dist: int;
}

let solved_cc = {
  _UD_flip = 0;
  _RL_flip = 0;
  _FB_flip = 0;
  _UD_twist = 0;
  _RL_twist = 0;
  _FB_twist = 0;
  _UD_slice_sorted = 0;
  _RL_slice_sorted = 0;
  _FB_slice_sorted = 0;
  corners = 0;
  _UD_dist = 0;
  _RL_dist = 0;
  _FB_dist = 0;
}

let tables = ref None

let load_tables () =
  match !tables with
  | Some tables -> tables
  | None ->
    let value = (load_conj_move (), load_corner_depth (),
    load_corners_move (), load_flip_move (), load_flipslicesorted_tables (),
    load_flipslicesorted_twist_depth3 (), load_slice_sorted_move (), load_twist_conj (),
    load_twist_move ()) in
    tables := Some value;
    value

(** does not work somehow *)
let next1 (cc: coord_cube) ((m, n): e_move * int): coord_cube * int =
  let python_array_access array i =
    array.(if i < 0 then Array.length array + i else i)
  in
  let (conj_move, corner_depth,
  corners_move, flip_move, (flipslicesorted_classidx, flipslicesorted_sym, flipslicesorted_rep),
  flipslicesorted_twist_depth3, slice_sorted_move, twist_conj,
  twist_move) = load_tables () in
  let h = ref 0 in
  let _UD_flip = cc._UD_flip in
  let _RL_flip = cc._RL_flip in
  let _FB_flip = cc._FB_flip in
  let _UD_twist = cc._UD_twist in
  let _RL_twist = cc._RL_twist in
  let _FB_twist = cc._FB_twist in
  let _UD_slice_sorted = cc._UD_slice_sorted in
  let _RL_slice_sorted = cc._RL_slice_sorted in
  let _FB_slice_sorted = cc._FB_slice_sorted in
  let corners = cc.corners in
  let _UD_dist = cc._UD_dist in
  let _RL_dist = cc._RL_dist in
  let _FB_dist = cc._FB_dist in
  let om = ordinal_e_move m * 3 + n mod 4 - 1 in
  (* ########################################################################################################### *)
  let corners1 = corners_move.(_N_MOVE * corners + om) in
  let co_dist1 = corner_depth.(corners1) in
  h := max !h co_dist1;
  
  (* ########################################################################################################### *)
  let _UD_twist1 = twist_move.(_N_MOVE * _UD_twist + om) in
  let _UD_flip1 = flip_move.(_N_MOVE * _UD_flip + om) in
  let _UD_slice_sorted1 = slice_sorted_move.(_N_MOVE * _UD_slice_sorted + om) in

  let fs = _N_FLIP * _UD_slice_sorted1 + _UD_flip1 in  (*  raw new flip_slicesorted coordinate *)
  (*  now representation as representant-symmetry pair *)
  let fs_idx = Int32.bits_of_float (Float.Array.get flipslicesorted_classidx fs) in
  let fs_sym = flipslicesorted_sym.(fs) in  (*  symmetry *)

  let _UD_dist1_mod3 = get_flipslicesorted_twist_depth3 flipslicesorted_twist_depth3
      (_N_TWIST * Int32.to_int fs_idx + twist_conj.((_UD_twist1 lsl 4) + fs_sym)) in
  let _UD_dist1 = python_array_access distance (3 * _UD_dist + _UD_dist1_mod3) in

  h := max !h _UD_dist1;
  
  (* ########################################################################################################### *)
  let mrl = conj_move.(_N_MOVE * 16 + om) in  (*  move viewed from 120° rotated position *)

  let _RL_twist1 = twist_move.(_N_MOVE * _RL_twist + mrl) in
  let _RL_flip1 = flip_move.(_N_MOVE * _RL_flip + mrl) in
  let _RL_slice_sorted1 = slice_sorted_move.(_N_MOVE * _RL_slice_sorted + mrl) in

  let fs = _N_FLIP * _RL_slice_sorted1 + _RL_flip1 in
  let fs_idx = Int32.bits_of_float (Float.Array.get flipslicesorted_classidx fs) in
  let fs_sym = flipslicesorted_sym.(fs) in

  let _RL_dist1_mod3 = get_flipslicesorted_twist_depth3 flipslicesorted_twist_depth3
      (_N_TWIST * Int32.to_int fs_idx + twist_conj.((_RL_twist1 lsl 4) + fs_sym)) in
  let _RL_dist1 = python_array_access distance (3 * _RL_dist + _RL_dist1_mod3) in

  h := max !h _RL_dist1;
  (* ########################################################################################################### *)
  let mfb = conj_move.(_N_MOVE * 32 + om) in  (*  move viewed from 240° rotated position *)

  let _FB_twist1 = twist_move.(_N_MOVE * _FB_twist + mfb) in
  let _FB_flip1 = flip_move.(_N_MOVE * _FB_flip + mfb) in
  let _FB_slice_sorted1 = slice_sorted_move.(_N_MOVE * _FB_slice_sorted + mfb) in

  let fs = _N_FLIP * _FB_slice_sorted1 + _FB_flip1 in
  let fs_idx = Int32.bits_of_float (Float.Array.get flipslicesorted_classidx fs) in
  let fs_sym = flipslicesorted_sym.(fs) in

  let _FB_dist1_mod3 = get_flipslicesorted_twist_depth3 flipslicesorted_twist_depth3
      (_N_TWIST * Int32.to_int fs_idx + twist_conj.((_FB_twist1 lsl 4) + fs_sym)) in
  let _FB_dist1 = python_array_access distance (3 * _FB_dist + _FB_dist1_mod3) in

  h := max !h _FB_dist1;
  (* ########################################################################################################### *)
  if _UD_dist1 <> 0 && _UD_dist1 = _RL_dist1 && _RL_dist1 = _FB_dist1
  then h := max !h (_UD_dist1 + 1);  (*  due to design of coordinate *)
  {_UD_flip=_UD_flip1; _RL_flip=_RL_flip1; _FB_flip=_FB_flip1; _UD_twist=_UD_twist1; _RL_twist=_RL_twist1; _FB_twist=_FB_twist1; _UD_slice_sorted=_UD_slice_sorted1;
      _RL_slice_sorted=_RL_slice_sorted1; _FB_slice_sorted=_FB_slice_sorted1; corners=corners1; _UD_dist=_UD_dist1; _RL_dist=_RL_dist1; _FB_dist=_FB_dist1}, !h


let next (cc: coord_cube) (md: int) (alg: e_alg): coord_cube * int =
  List.fold_left (fun (cc, _) m -> next1 cc m) (cc, md) alg

let cc_dist_of (cube: cube): coord_cube * int =
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 8084) in

  let sock = Unix.(socket PF_INET SOCK_STREAM 0) in
  Unix.connect sock addr;
  let in_ch = Unix.in_channel_of_descr sock
  and out_ch = Unix.out_channel_of_descr sock in

  let cube, _ = realign cube in

  output_string out_ch (cube_to_str cube ^ "\r\n");
  flush out_ch;

  let line = input_line in_ch in
  Unix.close sock;
  let alg = parse_e_alg (String.sub line 0 (String.index line '(')) in
  next solved_cc 0 alg


let rec vanilla_search_aux (cc: coord_cube) (togo: int) (sofar: e_alg): e_alg list =
  print_newline ();
  print_endline (e_alg_to_string sofar);
  if togo < 0 then []
  else if cc = solved_cc then
    [List.rev sofar]
  else if togo > 0 then
    all_e_moves |> List.fold_left (fun acc (m, n) ->
      match sofar with
      (*
      if len(self.sofar) > 0:
          diff = self.sofar[-1] // 3 - m // 3
          if diff in [0, 3]:  (*  successive moves on same face or on same axis with wrong order *)
            continue
      *)
      | (x, _)::_ when let v = ordinal_e_move x - ordinal_e_move m in v = 0 || v = 3 -> acc
      | _ ->
        let cc1, h = next1 cc (m, n) in
        if h > togo then acc
        else vanilla_search_aux cc1 (togo - 1) ((m, n)::sofar) @ acc
    ) []
  else []

(** why would you do that ? *)
let vanilla_search (depth_max: int) (cc: coord_cube) =
  vanilla_search_aux cc depth_max []


let my_search (move_times) (cc: coord_cube): (float * maneuver) list =
  let min_t = Hashtbl.fold (fun (_, alg, _) t acc -> if alg = [] then acc else min acc t) move_times 500. in
  print_endline "--------------------------------";
  let rec aux (cc: coord_cube) (alpha: float ref) (sofar: e_alg): (float * maneuver) list =
    if List.length sofar > 5 then []
    else if cc = solved_cc then
      begin
        let t, man = fastest_maneuver move_times (List.rev sofar) (id_rot ()) (id_rot ()) in
        alpha := t;
        [(t, man)]
      end
    else
      all_e_moves |> List.fold_left (fun acc (m, n) ->
        match sofar with
        | (x, _)::_ when let v = ordinal_e_move x - ordinal_e_move m in v = 0 || v = 3 -> acc
        | _ ->
          let cc1, h = next1 cc (m, n) in
          if min_t *. float_of_int (List.length sofar + h) >= !alpha then acc
          else aux cc1 alpha ((m, n)::sofar) @ acc
      ) []
  in
  aux cc (ref 4.0) []


let test_search alg =
  my_search (load_profile "default" |> Option.get |> load_move_times) (next solved_cc 0 (parse_e_alg alg) |> fst)

let test_search1 alg =
  vanilla_search 2 (next solved_cc 0 (parse_e_alg alg) |> fst)

