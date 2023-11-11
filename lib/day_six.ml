open Batteries

let read_lines filename =
  let contents = In_channel.with_open_bin filename In_channel.input_all in
  String.split_on_char '\n' contents
;;

let rec unique_string a_str =
  let initial_char = a_str.[0] in
  let end_string = String.tail a_str 1 in
  if String.length a_str <= 1
  then true
  else if String.contains end_string initial_char
  then false
  else unique_string end_string
;;

let rec find_signal counter sig_length a_str =
  match a_str with
  | "" -> failwith "You lose"
  | a_str ->
    if String.length a_str < sig_length
    then failwith "You lose"
    else if unique_string (String.head a_str sig_length)
    then counter + sig_length
    else find_signal (succ counter) sig_length (String.tail a_str 1)
;;

let main () =
  let filename = "./inputs/6.txt" in
  let input = read_lines filename |> List.hd in
  let outcome = find_signal 0 4 input in
  let outcome_2 = find_signal 0 14 input in
  Printf.printf "Part 1 total is %d\n" outcome;
  Printf.printf "Part 2 total is %d\n" outcome_2
;;
