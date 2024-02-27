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

(*  #################### Symmetry related functions. Symmetry considerations increase the performance of the solver.###### *)
  
let load_conj_move () =
    let fname = "conj_move" in
    print_endline ("loading " ^ fname ^ " table...");
    SMoves.load_uint16_array fname (SMoves._N_MOVE * SMoves._N_SYM)

(*  ###### Generate table for the conjugation of the twist t by a symmetry s. twist_conj[t, s] = s*t*s^-1 #### *)
let load_twist_conj () =
    let fname = "conj_twist" in
    print_endline ("loading " ^ fname ^ " table...");
    SMoves.load_uint16_array fname (SMoves._N_TWIST * SMoves._N_SYM_D4h)

(*  ###################################################################################################################### *)

(*  ############## Generate the tables to handle the symmetry reduced flip-slicesorted coordinate ######################## *)
(* let flipslicesorted_classidx, flipslicesorted_sym, flipslicesorted_rep = *)

let load_flipslicesorted_tables () =
    let fname1 = "fs24_classidx"
    and fname2 = "fs24_sym"
    and fname3 = "fs24_rep" in

    print_endline ("loading " ^ "flipslicesorted sym-tables...");

    SMoves.load_uint32_array fname1 (SMoves._N_FLIP * SMoves._N_SLICE_SORTED),
    SMoves.load_uint8_array fname2 (SMoves._N_FLIP * SMoves._N_SLICE_SORTED),
    SMoves.load_uint32_array fname3 (SMoves._N_FLIPSLICESORTED_CLASS)

(* ####################################################################################################################### *)
