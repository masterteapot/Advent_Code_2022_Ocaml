open Batteries

exception No_match_found of string

type crate_instruction =
  { num_crates : int
  ; start_pile : int
  ; end_pile : int
  }

(* read file line by line *)
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

let find_first_empty_string a_list =
  let rec find_an_empty x_list counter =
    match x_list with
    | [] -> raise (No_match_found "No empty string found in list")
    | "" :: _ -> counter
    | _ :: tl -> find_an_empty tl (counter + 1)
  in
  find_an_empty a_list 0
;;

let parse_level start_index interval a_str =
  let rec inner_level x_str interval result =
    match x_str with
    | "" -> result
    | x_str ->
      let inner_char = x_str.[0] in
      inner_char :: inner_level (String.tail x_str interval) interval result
  in
  inner_level (String.tail a_str start_index) interval []
;;

let rec parse_crates start_index interval a_list =
  match a_list with
  | [] -> []
  | hd :: tl ->
    parse_level start_index interval hd :: parse_crates start_index interval tl
;;

let rec drop_last a_list =
  match a_list with
  | [] -> []
  | _ :: [] -> []
  | hd :: tl -> hd :: drop_last tl
;;

let rec list_o_list a_list =
  match a_list with
  | [] -> []
  | hd :: tl -> [ hd ] :: list_o_list tl
;;

let rec put_list_in_list a_list b_list =
  match a_list with
  | [] -> []
  | hd :: tl -> (hd :: List.hd b_list) :: put_list_in_list tl (List.tl b_list)
;;

let sort_crate_piles a_list =
  let rec inner_lists x_list result =
    match x_list with
    | [] -> result
    | hd :: tl ->
      if List.is_empty result
      then inner_lists tl (list_o_list hd)
      else inner_lists tl (put_list_in_list hd result)
  in
  inner_lists a_list []
;;

let rec drop_space_chars a_list =
  match a_list with
  | [] -> []
  | hd :: tl -> if hd = ' ' then drop_space_chars tl else hd :: drop_space_chars tl
;;

let rec map_fun_inner_lists inner_fun a_list =
  match a_list with
  | [] -> []
  | hd :: tl -> inner_fun hd :: map_fun_inner_lists inner_fun tl
;;

let parse_instructions a_str =
  let pieces = String.split_on_char ' ' a_str in
  match pieces with
  | [ _; num_crates; _; start_pile; _; end_pile ] ->
    { num_crates = int_of_string num_crates
    ; start_pile = int_of_string start_pile
    ; end_pile = int_of_string end_pile
    }
  | _ -> failwith "you loser"
;;

let move_crates_p1 instruction crates =
  let movin_crates =
    List.take instruction.num_crates (List.nth crates (instruction.start_pile - 1))
  in
  let new_start =
    List.drop instruction.num_crates (List.nth crates (instruction.start_pile - 1))
  in
  let new_end = List.rev movin_crates @ List.nth crates (instruction.end_pile - 1) in
  let rec inner_crater instruction crates counter =
    match crates with
    | [] -> []
    | hd :: tl ->
      if counter = instruction.start_pile
      then new_start :: inner_crater instruction tl (succ counter)
      else if counter = instruction.end_pile
      then new_end :: inner_crater instruction tl (succ counter)
      else hd :: inner_crater instruction tl (succ counter)
  in
  inner_crater instruction crates 1
;;

let rec movin_all_crates_p1 instructions crates =
  match instructions with
  | [] -> crates
  | hd :: tl -> movin_all_crates_p1 tl (move_crates_p1 hd crates)
;;

let move_crates_p2 instruction crates =
  let movin_crates =
    List.take instruction.num_crates (List.nth crates (instruction.start_pile - 1))
  in
  let new_start =
    List.drop instruction.num_crates (List.nth crates (instruction.start_pile - 1))
  in
  let new_end = movin_crates @ List.nth crates (instruction.end_pile - 1) in
  let rec inner_crater instruction crates counter =
    match crates with
    | [] -> []
    | hd :: tl ->
      if counter = instruction.start_pile
      then new_start :: inner_crater instruction tl (succ counter)
      else if counter = instruction.end_pile
      then new_end :: inner_crater instruction tl (succ counter)
      else hd :: inner_crater instruction tl (succ counter)
  in
  inner_crater instruction crates 1
;;

let rec movin_all_crates_p2 instructions crates =
  match instructions with
  | [] -> crates
  | hd :: tl -> movin_all_crates_p2 tl (move_crates_p2 hd crates)
;;

let rec top_char a_list result =
  let suffix_char str ch = str ^ String.make 1 ch in
  match a_list with
  | [] -> result
  | hd :: tl -> top_char tl (suffix_char result (List.hd hd))
;;

let main () =
  let filename = "./inputs/5.txt" in
  let input = read_lines filename in
  let empty_index = find_first_empty_string input in
  let crates =
    BatList.take empty_index input
    |> parse_crates 1 4
    |> sort_crate_piles
    |> map_fun_inner_lists List.rev
    |> map_fun_inner_lists drop_space_chars
  in
  let instructions =
    BatList.drop empty_index input |> remove_empty_string |> List.map parse_instructions
  in
  let final_crates_p1 = movin_all_crates_p1 instructions crates in
  let outcome_1 = top_char final_crates_p1 "" in
  let final_crates_p2 = movin_all_crates_p2 instructions crates in
  let outcome_2 = top_char final_crates_p2 "" in
  Printf.printf "Part 1 answer is %s\n" outcome_1;
  Printf.printf "Part 2 answer is %s\n" outcome_2
;;
