open Rubiks_cube 
open CuberProfile
open AlgFinderElem
open Algorithm
open Permutation
open Cube

open GMain
open GdkKeysyms

module EAlgSet = Set.Make(struct type t = e_alg let compare = compare end)

let hand_pos_to_string ((h1, h2): hand_pos): string =
  let str_hp (hp: single_hand_pos): string = 
    match hp with
    | Af -> "Af" | F -> "F" | U -> "U" | Bu -> "B" | Bd -> "P" | D -> "D" | Out -> "O" | M -> "M"
  in
  str_hp h1 ^ ":" ^ str_hp h2

let maneuver_to_string maneuver =
  List.fold_left (fun acc (h1, a, h2) ->
    (if acc = "" then ("[" ^ hand_pos_to_string h1 ^ "] ") else acc) ^
    (
      match a with 
      | [] -> "[" ^ hand_pos_to_string h2 ^ "]"
      | [_] -> alg_to_string a
      | _ -> "(" ^ alg_to_string a ^ ")"
    ) ^ " "
  ) "" maneuver


let app_main () =
  let _ = GMain.init () in
  let window = GWindow.window ~width:1040 ~height:620
                          ~title:"Configuration du profil" () in
  window#connect#destroy ~callback:Main.quit |> ignore;

  let scroll = GBin.scrolled_window ~packing:window#add () in
  
  let vbox = GPack.vbox ~packing:scroll#add () in

  let line = GPack.hbox ~packing:(vbox#pack ~padding:5) () in
  let _ = GMisc.label ~packing:(line#pack ~padding:5) ~text:"Nom du profil: " () in
  let profile_name = GText.view ~accepts_tab:false ~editable:true ~packing:(line#pack ~expand:true ~padding:5) () in
  let profile_name_ok = GMisc.image ~stock:GtkStock.(`REFRESH) ~packing:(line#pack ~padding:5) () in
  let get_prof_name () = (profile_name#buffer#get_text ()) in
  let check_profile_name_ok () =
    if is_valid_profile_name (get_prof_name ()) then
      profile_name_ok#set_stock GtkStock.(`OK)
    else
      profile_name_ok#set_stock GtkStock.(`NO)
  in
  check_profile_name_ok ();
  profile_name#buffer#connect#end_user_action ~callback:(fun () -> 
    check_profile_name_ok ()
    ) |> ignore;
  let load_profile_button = GButton.button ~label:"Charger" ~packing:(line#pack ~padding:5) () in
  let save_profile_button = GButton.button ~label:"Sauvegarder" ~packing:(line#pack ~padding:5) () in
  
  let line = GPack.hbox ~packing:(vbox#pack ~padding:5) () in
  let _ = GMisc.label ~packing:(line#pack ~padding:5) ~text:"Aisance U2 droit (unité: nombre de fois U): " () in
  let r_ease_U2 = GData.adjustment ~value:1.2 ~lower:1. ~upper:2. ~page_size:0. ~page_incr:0. () in
  let _ = GRange.scale `HORIZONTAL ~adjustment:r_ease_U2 ~digits:2 ~packing:(line#pack ~expand:true ~padding:5) () in

  let line = GPack.hbox ~packing:(vbox#pack ~padding:5) () in
  let _ = GMisc.label ~packing:(line#pack ~padding:5) ~text:"Aisance U2 gauche (unité: nombre de fois U'): " () in
  let l_ease_U2 = GData.adjustment ~value:1.2 ~lower:1. ~upper:2. ~page_size:0. ~page_incr:0. () in
  let _ = GRange.scale `HORIZONTAL ~adjustment:l_ease_U2 ~digits:2 ~packing:(line#pack ~expand:true ~padding:5) () in
  
  let line = GPack.hbox ~packing:(vbox#pack ~padding:5) () in
  let _ = GMisc.label ~packing:(line#pack ~padding:5) ~text:"Main utilisé pour les mouvements M: " () in
  let hand_M_right = GButton.radio_button ~label:"droite" ~packing:line#pack () in
  let hand_M_left = GButton.radio_button ~group:hand_M_right#group ~label:"gauche"
      ~active:true ~packing:line#pack () in

  let line = GPack.hbox ~packing:(vbox#pack ~padding:5) () in
  let _ = GMisc.label ~packing:(line#pack ~padding:5) ~text:"Aisance M':                           " () in
  let ease_M' = GData.adjustment ~value:1.2 ~lower:1. ~upper:2. ~page_size:0. ~page_incr:0. () in
  let _' = GRange.scale `HORIZONTAL ~adjustment:ease_M' ~digits:2 ~packing:(line#pack ~expand:true ~padding:5) () in

  let line = GPack.hbox ~packing:(vbox#pack ~padding:5) () in
  let _ = GMisc.label ~packing:(line#pack ~padding:5) ~text:"Aisance M2 (unité: nombre de fois M'): " () in
  let ease_M2 = GData.adjustment ~value:1.2 ~lower:1. ~upper:2. ~page_size:0. ~page_incr:0. () in
  let _ = GRange.scale `HORIZONTAL ~adjustment:ease_M2 ~digits:2 ~packing:(line#pack ~expand:true ~padding:5) () in

  let line = GPack.hbox ~packing:(vbox#pack ~padding:5) () in
  let _ = GMisc.label ~packing:(line#pack ~padding:5) ~text:"Aisance M (unité: nombre de fois M'):  " () in
  let ease_M = GData.adjustment ~value:1.2 ~lower:1. ~upper:2. ~page_size:0. ~page_incr:0. () in
  let _ = GRange.scale `HORIZONTAL ~adjustment:ease_M ~digits:2 ~packing:(line#pack ~expand:true ~padding:5) () in
  
  let line = GPack.hbox ~packing:(vbox#pack ~padding:5) () in
  let delete_profile_button = GButton.button ~label:"Supprimer" ~packing:(line#pack ~padding:5) () in
  let reset_profile_button = GButton.button ~label:"Réinitialiser" ~packing:(line#pack ~padding:5) () in
  let alg_text_zone = GText.view ~accepts_tab:false ~editable:true ~border_width:2 ~packing:(line#pack ~expand:true ~padding:5) () in
  (* let wait_icon = GMisc.image ~packing:(line#pack ~padding:5) () in *)
  let test_button = GButton.button ~label:"Test" ~packing:(line#pack ~padding:5) () in
  (*let stop_button = GButton.button ~label:"Stop" ~packing:(line#pack ~padding:5) () in
  let stopask_button = GButton.button ~label:"Stop ask" ~packing:(line#pack ~padding:5) () in*)

  let line = GPack.hbox ~packing:(vbox#pack ~padding:5) () in
  let _ = GMisc.label ~packing:(line#pack ~padding:5) ~text:"Générateur:" () in
  let result1_text = GText.view ~editable:false ~packing:(line#pack ~padding:5) () in
  let _ = GMisc.separator `VERTICAL ~packing:(line#pack ~padding:5) () in
  let _ = GMisc.label ~packing:(line#pack ~padding:5) ~text:"Solution:" () in
  let result2_text = GText.view ~editable:false ~packing:(line#pack ~padding:5) () in
  
  
  let apply_profile prof =
    r_ease_U2#set_value prof.ease_right_U2;
    l_ease_U2#set_value prof.ease_left_U2;
    hand_M_right#set_active (prof.hand_M = R);
    hand_M_left#set_active (prof.hand_M = L);
    ease_M'#set_value prof.ease_M';
    ease_M2#set_value prof.ease_M2;
    ease_M#set_value prof.ease_M
  in
  apply_profile default_profile;
  let get_current_profile () =
    {
        ease_right_U2 = r_ease_U2#value;
        ease_left_U2 = l_ease_U2#value;
        hand_M = (if hand_M_right#active then R else L);
        ease_M' = ease_M'#value;
        ease_M2 = ease_M2#value;
        ease_M = ease_M#value;
      }
  in

  load_profile_button#connect#clicked ~callback:(fun () -> 
    if is_valid_profile_name (get_prof_name ()) || get_prof_name () = "default" then
      begin
        let prof = load_profile (get_prof_name ()) in
        match prof with
        | Some prof ->  apply_profile prof
        | None ->
          let dialog = GWindow.message_dialog ~title:"Profil inexistant" ~message_type:`WARNING
          ~message:"Le profil n'existe pas."
          ~buttons:GWindow.Buttons.ok ()
          in
          match dialog#run () with
          | `OK | `DELETE_EVENT -> dialog#destroy ()
      end
    else
      let dialog = GWindow.message_dialog ~title:"Profil invalide" ~message_type:`WARNING
          ~message:"Le nom du profil est invalide, il ne doit contenir que des charactères en minuscules (a-z), des underscores (_) et des chiffres (0-9)."
          ~buttons:GWindow.Buttons.ok ()
      in
      match dialog#run () with
      | `OK | `DELETE_EVENT -> dialog#destroy ()
  ) |> ignore;
  
  save_profile_button#connect#clicked ~callback:(fun () -> 
    if is_valid_profile_name (get_prof_name ()) then
      begin
        save_profile (get_prof_name ()) (get_current_profile ());
      let dialog = GWindow.message_dialog ~title:"Profil sauvegardé" ~message_type:`INFO
        ~message:"Le profil a été sauvegardé avec succès"
        ~buttons:GWindow.Buttons.ok ()
      in
      match dialog#run () with
      | _ -> dialog#destroy ()
      end
    else
      let dialog = GWindow.message_dialog ~title:"Profil invalide" ~message_type:`WARNING
          ~message:"Le nom du profil est invalide, il ne doit contenir que des charactères en minuscules (a-z), des underscores (_) et des chiffres (0-9)."
          ~buttons:GWindow.Buttons.ok ()
      in
      match dialog#run () with
      | _ -> dialog#destroy ()
  ) |> ignore;

  delete_profile_button#connect#clicked ~callback:(fun () -> 
    if is_valid_profile_name (get_prof_name ()) then
      if delete_profile (get_prof_name ()) then
        let dialog = GWindow.message_dialog ~title:"Profil supprimé" ~message_type:`INFO
          ~message:"Le profil a été supprimé avec succès"
          ~buttons:GWindow.Buttons.ok ()
        in
        match dialog#run () with
        | _ -> dialog#destroy ()
  ) |> ignore;

  reset_profile_button#connect#clicked ~callback:(fun () ->
    apply_profile default_profile
  ) |> ignore;

  test_button#connect#clicked ~callback:(fun () ->
    result1_text#buffer#set_text "...";
    result2_text#buffer#set_text "...";

    let str = alg_text_zone#buffer#get_text () in
    let prof = get_current_profile () in
    let move_times = load_move_times prof in
    Thread.create (fun () ->
      let l = AlgEvaluatorElem.fastest_maneuvers ~timeout:2.0 ~heuristic:0.2 1 move_times (parse_e_alg str) (id_rot ()) (id_rot ()) in
      List.fold_left (fun acc (t, maneuver) ->
        let alg_str = maneuver_to_string maneuver in
        acc ^ alg_str ^ string_of_float t ^ "\n"
      ) "" l
      |> result1_text#buffer#set_text
    ) () |> ignore;

    Thread.create (fun () -> (* RUR'U'R'FR2U'R'U'RUR'F'  ~quotient_group:pll_subgroup  *)
      let cube = cube_from_alg (parse_alg str) in
      let l = fastest_maneuvers ~heuristic:0.1 5 move_times (inverse_cube cube) in
      List.fold_left (fun acc (t, maneuver) ->
        let alg_str = maneuver_to_string maneuver in
        acc ^ alg_str ^ string_of_float t ^ "\n"
      ) "" l
      |> result2_text#buffer#set_text
    ) () |> ignore;
  ) |> ignore;

  window#show ();
  GMain.main ()

let _ = app_main ()

(*  
   test_button#set_sensitive true;

  let computer_stop = ref false in
  let asker_stop = ref false in
  let terminator = ref ignore in
  let asker_thread = ref None in
  let computer_thread = ref None in

  test_button#connect#clicked ~callback:(fun () ->
    result1_text#buffer#set_text "...";
    result2_text#buffer#set_text "...";

    let str = alg_text_zone#buffer#get_text () in
    let prof = get_current_profile () in
    let move_times = load_move_times prof in
    Thread.create (fun () ->
      let t, _, hist = fastest_maneuver move_times (parse_alg str |> elementalize_no_rot) (id_rot ()) (id_rot ()) in
      let alg_str = (List.fold_left (fun acc (_, a, _) -> acc ^ alg_to_string a ^ " ") "" hist) in
      result1_text#buffer#set_text (alg_str ^ string_of_float t)
    ) () |> ignore;
    
    
    let algs = ref [] in
    let cube = cube_from_alg (parse_alg str |> inverse_alg) in
    let finished = ref false in
    let to_chain_queue = Queue.create () in
    let algs_seen = ref EAlgSet.empty in
    let lock = Mutex.create () in
    
    test_button#set_sensitive false;
    wait_icon#set_stock GtkStock.(`CDROM);

    computer_thread := Some (Thread.create (fun () ->
      while (not !finished || not (Queue.is_empty to_chain_queue)) && not !computer_stop do
        Mutex.lock lock;
        match Queue.take_opt to_chain_queue with
        | None -> Mutex.unlock lock; Thread.delay 0.5
        | Some x ->
          Mutex.unlock lock;
          begin
            print_endline ("alg " ^ e_alg_to_string ~space:false x ^ " : ");
            fastest_maneuvers ~timeout:0.3 ~heuristic:0.165 5 move_times x (id_rot ()) (id_rot ())
            |> List.iter (fun (t, _, maneuver) ->
              let alg_str = 
                List.fold_left (fun acc (h1, a, h2) ->
                  (if acc = "" then ("[" ^ hand_pos_to_string h1 ^ "] ") else acc) ^
                  (if a = [] then "[" ^ hand_pos_to_string h2 ^ "]" else alg_to_string a) ^ " "
                ) "" maneuver
              in
              print_endline (" computed " ^ alg_str ^ " for " ^ string_of_float t);
              let me = (alg_str, t) in
              let rec insert_me (l: (string * float) list) =
                match l with
                | [] -> [me]
                | (s, t')::q ->
                  if s = alg_str then
                    if t < t' then me::q
                    else l
                  else
                    if t < t' then me::l
                    else (s, t')::insert_me q
              in
              algs := insert_me !algs;
              let res_txt = List.fold_left (fun acc (alg, t) -> acc ^ alg ^ string_of_float t ^ "\n") "" !algs in
              result2_text#buffer#set_text res_txt
              );
            print_endline ("alg done")
          end;
      done;
      test_button#set_sensitive true;
      wait_icon#set_stock GtkStock.(`YES)
    ) ());
    let thr, term = ask_solves_async cube 20 (fun solution ->
      let solution = e_simplify solution in
      if not (EAlgSet.mem solution !algs_seen) then
        begin
          print_endline ("received " ^ e_alg_to_string solution);
          Mutex.lock lock;
          Queue.add solution to_chain_queue;
          algs_seen := EAlgSet.add solution !algs_seen;
          Mutex.unlock lock;
        end;
      not !asker_stop
    ) (fun () -> finished := true)
    in
    asker_thread := Some thr;
    terminator := term
  ) |> ignore;

  stop_button#connect#clicked ~callback:(fun () ->
    computer_stop := true;
    match !computer_thread with
    | None -> ()
    | Some thr -> Thread.join thr;
    computer_thread := None;
    computer_stop := false;

    !terminator ();
    terminator := ignore;
    asker_stop := true;
    match !asker_thread with
    | None -> ()
    | Some thr -> Thread.join thr;
    asker_thread := None;
    asker_stop := false

  ) |> ignore;
  stopask_button#connect#clicked ~callback:(fun () ->
      !terminator ();
      terminator := ignore;
      asker_stop := true;
      match !asker_thread with
      | None -> ()
      | Some thr -> Thread.join thr;
      asker_stop := false
    ) |> ignore;
*)