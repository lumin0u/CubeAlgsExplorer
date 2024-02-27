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

let _N_PERM_4 = 24
let _N_MOVE = 18  (*  number of possible face moves *)

let _N_TWIST = 2187  (*  3^7 possible corner orientations *)
let _N_FLIP = 2048  (*  2^11 possible edge orientations *)
let _N_SLICE_SORTED = 11880  (*  12*11*10*9 possible positions of the edges FR, FL, BL, BR *)
let _N_SLICE = _N_SLICE_SORTED / _N_PERM_4  (*  we ignore the permutation of FR, FL, BL, BR *)
let _N_FLIPSLICE_CLASS = 64430  (*  number of equivalence classes for combined flip+slice concerning symmetry group D4h *)
let _N_FLIPSLICESORTED_CLASS = 1523864  (*  equivalence classes for combined flip+slicesorted concerning symmetry group D4h *)

let _N_CORNERS = 40320  (*  8! corner permutations *)
let _N_CORNERS_CLASS = 2768  (*  number of equivalence classes concerning symmetry group D4h *)

let _N_SYM = 48  (*  number of cube symmetries of full group Oh *)
let _N_SYM_D4h = 16  (*  Number of symmetries of subgroup D4h *)


let load_uint16_array (file: string) (len: int): int array =
  let fh = open_in_bin file in
  let buf = Bytes.create (len * 2) in
  let _ = input fh buf 0 (len * 2) / 2 in
  let array = Array.make len 0 in
  for i = 0 to len - 1 do
    array.(i) <- Bytes.get_uint16_le buf (i * 2);
  done;
  close_in fh;
  array

let load_uint32_array (file: string) (len: int): floatarray =
  let fh = open_in_bin file in
  let buf = Bytes.create (len * 4) in
  let _ = input fh buf 0 (len * 4) / 4 in
  let array = Float.Array.create len in
  for i = 0 to len - 1 do
    Float.Array.set array i (Int32.float_of_bits (Bytes.get_int32_le buf (i * 4)));
  done;
  close_in fh;
  array

let load_uint8_array (file: string) (len: int): int array =
  let fh = open_in_bin file in
  let buf = Bytes.create (len * 1) in
  let _ = input fh buf 0 (len * 1) / 1 in
  let array = Array.make len 0 in
  for i = 0 to len - 1 do
    array.(i) <- Bytes.get_uint8 buf (i * 1);
  done;
  close_in fh;
  array

let load_int8_array (file: string) (len: int): int array =
  let fh = open_in_bin file in
  let buf = Bytes.create (len * 1) in
  let _ = input fh buf 0 (len * 1) / 1 in
  let array = Array.make len 0 in
  for i = 0 to len - 1 do
    array.(i) <- Bytes.get_int8 buf (i * 1);
  done;
  close_in fh;
  array
  

(*  ######################################### Move table for the twists of the corners. ################################## *)

let load_twist_move () =
  (*  The twist coordinate describes the 3^7 = 2187 possible orientations of the 8 corners *)
  (*  0 <= twist < 2187 *)
  let fname = "move_twist" in
  print_endline ("loading " ^ fname ^ " table...");
  load_uint16_array fname (_N_TWIST * _N_MOVE)

(*  ####################################  Move table for the flip of the edges. ########################################## *)

let load_flip_move () =
  (*  The flip coordinate describes the 2^11 = 2048 possible orientations of the 12 edges *)
  (*  0 <= flip < 2048 *)
  let fname = "move_flip" in
  print_endline ("loading " ^ fname ^ " table...");
  load_uint16_array fname (_N_FLIP * _N_MOVE)

(*  ###################### Move table for the four UD-slice edges FR, FL, Bl and BR. ##################################### *)

let load_slice_sorted_move () =
  (*  The slice_sorted coordinate describes the 12!/8! = 11880 possible positions of the FR, FL, BL and BR edges. *)
  (*  0 <= slice_sorted < 11880 *)
  let fname = "move_slice_sorted" in
  print_endline ("loading " ^ fname ^ " table...");
  load_uint16_array fname (_N_SLICE_SORTED * _N_MOVE)
  
(*  ########################################## Move table for the corners. ############################################### *)

let load_corners_move () =
  (*  The corners coordinate describes the 8! = 40320 permutations of the corners. *)
  (*  0 <= corners < 40320 *)
  let fname = "move_corners" in
  print_endline ("loading " ^ fname ^ " table...");
  load_uint16_array fname (_N_CORNERS * _N_MOVE)
