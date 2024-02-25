open Rubiks_cube
open Algorithm
open Permutation


let cross (l1: 'a list) (l2: 'b list): ('a * 'b) list =
  List.fold_left (fun acc x -> (List.fold_left (fun acc y -> (x, y)::acc) acc l2)) [] l1

let cross_f (f: 'a -> 'b -> 'c) (l1: 'a list) (l2: 'b list): 'c list =
  List.fold_left (fun acc x -> (List.map (f x) l2) @ acc) [] l1


let ask_a_solve (cube: cube) ?(goal: cube option) ?(conjugation = id_alg) ?(ask_inv = false) (maxlen: int): e_alg =
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 8080) in

  let sock = Unix.(socket PF_INET SOCK_STREAM 0) in
  Unix.connect sock addr;
  let in_ch = Unix.in_channel_of_descr sock
  and out_ch = Unix.out_channel_of_descr sock in

  let maxlen = max 0 (maxlen - 2 * List.length conjugation) in
  let inv_conj = inverse_e_alg conjugation in

  let goal = match goal with | Some goal -> goal | None -> get_id_cube () in
  let cube = apply_e_alg cube conjugation in
  let goal = apply_e_alg goal conjugation in
  let (cube, goal) = if ask_inv then (goal, cube) else (cube, goal) in

  output_string out_ch (cube_to_str cube ^ ">" ^ cube_to_str goal ^ "|" ^ string_of_int maxlen ^ "\r\n");
  flush out_ch;

  let alg = input_line in_ch in
  print_endline alg;
  let alg = parse_e_alg (String.sub alg 0 (String.index alg '(')) in
  conjugation @ (if ask_inv then inverse_e_alg alg else alg) @ inv_conj


(* will ask for n solves for each conjugation/inverse *)
let ask_solves_async_try_everything (cube: cube) ?(goal: cube option) (n: int) (callback: e_alg -> unit) (finish: unit -> unit): Thread.t =
  Thread.create (fun () ->
    let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 8080) in

    let sock = Unix.(socket PF_INET SOCK_STREAM 0) in
    Unix.connect sock addr;
    let in_ch = Unix.in_channel_of_descr sock
    and out_ch = Unix.out_channel_of_descr sock in
    
    let rec search (adepth: int) (conj_s: e_alg) (conj_e: e_alg) (ask_inv: bool): unit =
      if adepth <= 0 then
        begin
          let goal = match goal with | Some goal -> goal | None -> get_id_cube () in
          let cube = apply_e_alg cube conj_s in
          let goal = apply_e_alg goal conj_e in
          let (cube, goal) = if ask_inv then (goal, cube) else (cube, goal) in
          
          print_endline ("CONJUGATE : " ^ e_alg_to_string ~space:false conj_s ^ " | " ^ e_alg_to_string ~space:false conj_e);
          for i = 0 to n-1 do
            output_string out_ch (cube_to_str cube ^ ">" ^ cube_to_str goal ^ "|" ^ string_of_int (Random.int 20 + 1) ^ "\r\n");
            flush out_ch;
          
            let alg = input_line in_ch in
            (* print_endline alg; *)
            let alg = parse_e_alg (String.sub alg 0 (String.index alg '(')) in
            callback (conj_s @ (if ask_inv then inverse_e_alg alg else alg) @ inverse_e_alg conj_e)
          done
        end
      else
        begin
          List.iter (fun m -> search (adepth - 1) (m::conj_s) conj_e ask_inv) ([R; F; L; U]
          |> List.map (fun c -> [(c, 1); (c, 2); (c, 3)])
          |> List.flatten);
          List.iter (fun m -> search (adepth - 1) conj_s (m::conj_e) ask_inv) ([R; F; L; U]
          |> List.map (fun c -> [(c, 1); (c, 2); (c, 3)])
          |> List.flatten)
        end
    in

    for depth = 0 to 6 do
      search depth [] [] false;
      search depth [] [] true;
    done;
    finish ()
  ) ()

