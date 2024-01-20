open Batteries

let read_lines filename =
  let contents = In_channel.with_open_bin filename In_channel.input_all in
  String.split_on_char '\n' contents
;;

let rec remove_empty_string a_list =
  match a_list with
  | [] -> []
  | a_str :: tl ->
    (match a_str with
     | "" -> remove_empty_string tl
     | a_str -> a_str :: remove_empty_string tl)
;;

let rec pp a_list =
  match a_list with
  | [] -> print_endline "\n"
  | hd :: tl ->
    Printf.printf "\n%s[ " "";
    List.iter (Printf.printf "%d; ") hd;
    Printf.printf "%s]" "";
    pp tl
;;

(* Zero indexed *)
let index_multi_list x y (deep_list : int list list) =
  let inner_list = List.nth deep_list y in
  List.nth inner_list x
;;

let rec get_all_verti_values x (deep_list : int list list) =
  match deep_list with
  | [] -> []
  | hd :: tl -> List.nth hd x :: get_all_verti_values x tl
;;

let find_view_path x y (deep_list : int list list) =
  let hori = List.nth deep_list y |> List.remove_at x in
  let verti = get_all_verti_values x deep_list |> List.remove_at y in
  let top = List.take y verti in
  let right = List.drop x hori in
  let bottom = List.drop y verti in
  let left = List.take x hori in
  top, right, bottom, left
;;

let is_hidden v path =
  let no_taller x = x >= v in
  let t = List.exists no_taller (Tuple4.first path) in
  let r = List.exists no_taller (Tuple4.second path) in
  let b = List.exists no_taller (Tuple4.third path) in
  let k = List.exists no_taller (Tuple4.fourth path) in
  t && r && b && k
;;

let walker deep_list =
  let rec helper_loop x y w h deep_list acc =
    let v = index_multi_list x y deep_list in
    let p = find_view_path x y deep_list in
    let r = is_hidden v p in
    let end_hori = x + 1 = w in
    let end_verti = y + 1 = h in
    match end_hori, end_verti, r with
    | true, true, r -> r :: acc
    | true, _, r -> helper_loop 0 (y + 1) w h deep_list (r :: acc)
    | false, _, r -> helper_loop (x + 1) y w h deep_list (r :: acc)
  in
  helper_loop 0 0 (List.length (List.hd deep_list)) (List.length deep_list) deep_list []
;;

let scenic_score v path =
  let rec no_taller v a_list counter =
    match a_list with
    | [] -> counter
    | hd :: _ when hd >= v -> counter + 1
    | _ :: tl -> no_taller v tl (counter + 1)
  in
  let t = no_taller v (List.rev (Tuple4.first path)) 0 in
  let r = no_taller v (Tuple4.second path) 0 in
  let b = no_taller v (Tuple4.third path) 0 in
  let l = no_taller v (List.rev (Tuple4.fourth path)) 0 in
  t * r * b * l
;;

let walker_v2 deep_list =
  let rec helper_loop x y w h deep_list acc =
    let v = index_multi_list x y deep_list in
    let p = find_view_path x y deep_list in
    let ss = scenic_score v p in
    let end_hori = x + 1 = w in
    let end_verti = y + 1 = h in
    match end_hori, end_verti, ss with
    | true, true, r -> r :: acc
    | true, _, r -> helper_loop 0 (y + 1) w h deep_list (r :: acc)
    | false, _, r -> helper_loop (x + 1) y w h deep_list (r :: acc)
  in
  helper_loop 0 0 (List.length (List.hd deep_list)) (List.length deep_list) deep_list []
;;

let input =
  read_lines "inputs/8.txt"
  |> remove_empty_string
  |> List.map (fun x ->
    String.explode x |> List.map (String.make 1) |> List.map int_of_string)
;;

(* Part 1 *)
let bool_vals = walker input
let result_1 = List.filter (fun x -> x = false) bool_vals |> List.length
let outcome_1 = result_1

(* Part 2 *)

let scenic_values = walker_v2 input
let outcome_2 = List.max scenic_values

let main () =
  Printf.printf "Part 1 total is %d\n" outcome_1;
  Printf.printf "Part 2 total is %d\n" outcome_2
;;
