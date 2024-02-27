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

(*  ##################### The pruning tables cut the search tree during the search. ###################################### *)
(*  ############ The pruning values are stored modulo 3 which saves a lot of memory. ##################################### *)

let load_flipslicesorted_twist_depth3 () =
  (* Create/load the flipslicesorted_twist_depth3 pruning table, 24x the phase1 table of the two-phase alg. *)
  let total = SMoves._N_FLIPSLICESORTED_CLASS * SMoves._N_TWIST in
  let fname = "phase1x24_prun" in
  print_endline ("loading " ^ fname ^ " table...");
  SMoves.load_uint32_array fname (total / 16 + 1)


let load_corner_depth () =
  (* Create/load the corner_depth pruning table. Entry gives the number of moves which are at least necessary
  to restore the corners. *)
  let fname = "cornerprun" in
  print_endline ("loading " ^ fname ^ " table...");
  SMoves.load_int8_array fname (SMoves._N_CORNERS)


(*  # array distance computes the new distance from the old_distance i and the new_distance_mod3 j. ###################### *)
(*  # We need this array because the pruning tables only store the distances mod 3. ###################################### *)
(*  # The advantage of storing distances mod 3 is that we need only 2 bit per entry to store values 0, 1 or 2 and still *)
(*  # have value 3 left to indicate a still empty entry in the tables during table creation. *)
let distance =
  let a = Array.make 60 0 in
  for i = 0 to 19 do
    for j = 0 to 2 do
      a.(3 * i + j) <- (i / 3) * 3 + j;
      if i mod 3 = 2 && j = 0 then
        a.(3 * i + j) <- a.(3 * i + j) + 3
      else if i mod 3 = 0 && j = 2 then
        a.(3 * i + j) <- a.(3 * i + j) - 3
    done
  done;
  a

let get_flipslicesorted_twist_depth3 flipslicesorted_twist_depth3 ix =
  (* get_fsst_depth3(ix) is *exactly* the number of moves % 3 to solve phase1x24 of a cube with index ix *)
  let y = Int32.bits_of_float (Float.Array.get flipslicesorted_twist_depth3 (ix / 16)) in
  let y = Int32.shift_right y ((ix mod 16) * 2) in
  (Int32.to_int y) land 3


(*
let set_flipslicesorted_twist_depth3 ix value =
  let shift = (ix mod 16) * 2 in
  let base = ix lsr 4 in
  flipslicesorted_twist_depth3.(base) <- flipslicesorted_twist_depth3.(base) land (lnot (3 lsl shift)) land 0xffffffff;
  flipslicesorted_twist_depth3.(base) <- flipslicesorted_twist_depth3.(base) lor (value lsl shift)
*)