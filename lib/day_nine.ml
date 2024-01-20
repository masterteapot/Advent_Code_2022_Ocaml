open Batteries

let rec remove_empty_string a_list =
  match a_list with
  | [] -> []
  | a_str :: tl ->
    (match a_str with
     | "" -> remove_empty_string tl
     | a_str -> a_str :: remove_empty_string tl)
;;

let read_lines filename =
  let contents = In_channel.with_open_bin filename In_channel.input_all in
  String.split_on_char '\n' contents
;;

let input =
  read_lines "inputs/8.txt"
  |> remove_empty_string
  |> List.map (fun x ->
    String.explode x |> List.map (String.make 1) |> List.map int_of_string)
;;

(* Part 1 *)
let outcome_1 = 1

(* Part 2 *)
let outcome_2 = 2

let main () =
  Printf.printf "Part 1 total is %d\n" outcome_1;
  Printf.printf "Part 2 total is %d\n" outcome_2
;;
