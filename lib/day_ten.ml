open Batteries

type instruction =
  | Addx
  | Noop

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

let parse_input a_str =
  let split = String.split_on_char ' ' a_str in
  match split with
  | "addx" :: tl ->
    ( Addx
    , (match tl with
       | hd :: _ -> int_of_string hd
       | _ -> failwith "No value found in addx instruction") )
  | "noop" :: _ -> Noop, 0
  | _ -> failwith "Not expected instruction format"
;;

let run_instructions instructions =
  let rec looper instructions queue inner_counter outer_counter value cycles =
    match queue, inner_counter with
    | (Noop, _), _ ->
      (match instructions with
       | [] -> List.rev ((outer_counter, value) :: cycles)
       | hd :: tl ->
         looper tl hd 0 (outer_counter + 1) value ((outer_counter, value) :: cycles))
    | (Addx, v), ic when ic = 1 ->
      (match instructions with
       | [] -> List.rev ((outer_counter, value + v) :: cycles)
       | hd :: tl ->
         looper
           tl
           hd
           0
           (outer_counter + 1)
           (value + v)
           ((outer_counter, value + v) :: cycles))
    | (Addx, _), ic when ic <> 1 ->
      looper
        instructions
        queue
        (inner_counter + 1)
        (outer_counter + 1)
        value
        ((outer_counter, value) :: cycles)
    | (Addx, _), _ ->
      failwith
        "We shouldn't he here. Maybe inner counter is broken or unexpected Addx value"
  in
  looper instructions (Noop, 0) 0 1 1 []
;;

let filter_signals signals =
  List.filter (fun x -> (fst x = 20 || Int.rem ((fst x) - 20) 40 = 0) && fst x < 221) signals
;;

let signal_strength signals =
  let rec looper signals output =
    match signals with
    | [] -> output
    | hd :: tl -> looper tl (output + (fst hd * snd hd))
  in
  looper signals 0
;;

let input = read_lines "inputs/10_test.txt" |> remove_empty_string |> List.map parse_input
let output1 = run_instructions input |> filter_signals;;

List.iter (fun x -> Printf.printf "%d -> %d\n" (fst x) (snd x)) output1;;
List.iter (fun x -> Printf.printf "%d, %d --> %d\n" (fst x) (snd x) (fst x * snd x)) output1

let output1 = run_instructions input |> filter_signals |> signal_strength;;

let outcome_1 = 1
let outcome_2 = 2

let main () =
  Printf.printf "Part 1 total is %d\n" outcome_1;
  Printf.printf "Part 2 total is %d\n" outcome_2
;;
