open Rubiks_cube
open Algorithm
open Permutation

let ask_solves_async (cube: cube) (depth: int) (callback: e_alg -> bool) (finish: unit -> unit): Thread.t * (unit -> unit) =
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 8085) in

  let sock = Unix.(socket PF_INET SOCK_STREAM 0) in
  Unix.connect sock addr;
  let in_ch = Unix.in_channel_of_descr sock
  and out_ch = Unix.out_channel_of_descr sock in

  Thread.create (fun (in_ch, out_ch) ->
    (*let in_ch, out_ch = 
      match !py_in_out_ch with
      | None -> start_py_script (); Option.get !py_in_out_ch
      | Some ch -> ch
    in*)
    output_string out_ch (cube_to_str cube ^ "|" ^ string_of_int depth ^ "\n");
    flush out_ch;
    try
      while
        let line = input_line in_ch in
        if line = "end" then false
        else begin
          let alg = parse_e_alg line in
          callback alg
        end
       do () done
    with
    | _ -> finish ()
  ) (in_ch, out_ch), (fun () -> Unix.close sock)

