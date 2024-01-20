open Batteries

type direction =
  | Right
  | Down
  | Left
  | Up

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

let split_on_space commands =
  let rec looper commands agg =
    match commands with
    | [] -> List.rev agg
    | hd :: tl ->
      let res_ls = String.split_on_char ' ' hd in
      let res =
        match res_ls with
        | [ dir; num ] ->
          let way =
            match dir with
            | "R" -> Right
            | "U" -> Up
            | "L" -> Left
            | "D" -> Down
            | _ -> failwith "Not a valid direction"
          in
          way, int_of_string num
        | _ -> failwith "This isn't the right format"
      in
      looper tl (res :: agg)
  in
  looper commands []
;;

let move_your_tail head tail =
  let x_diff = fst tail - fst head in
  let y_diff = snd tail - snd head in
  let x_dir =
    match x_diff with
    | _ when x_diff > 0 -> -1
    | _ when x_diff < 0 -> 1
    | _ -> 0
  in
  let y_dir =
    match y_diff with
    | 0 -> 0
    | _ when y_diff > 0 -> -1
    | _ when y_diff < 0 -> 1
    | _ -> 0
  in
  match x_diff, y_diff with
  | x, y when abs x = 0 && abs y = 2 -> fst tail, (1 * y_dir) + snd tail
  | x, y when abs x = 2 && abs y = 0 -> (1 * x_dir) + fst tail, snd tail
  | x, y when abs x = 1 && abs y = 2 -> (1 * x_dir) + fst tail, (1 * y_dir) + snd tail
  | x, y when abs x = 2 && abs y = 1 -> (1 * x_dir) + fst tail, (1 * y_dir) + snd tail
  | _ -> fst tail, snd tail
;;

let follow_instructions loc instructions =
  match instructions with
  | Up -> fst loc, snd loc + 1
  | Right -> fst loc + 1, snd loc
  | Down -> fst loc, snd loc - 1
  | Left -> fst loc - 1, snd loc
;;

let move_the_rope head tail instructions =
  let rec looper head tail instructions tail_locations =
    match instructions with
    | dir, num when num > 0 ->
      let new_head = follow_instructions head dir in
      let new_tail = move_your_tail new_head tail in
      looper new_head new_tail (dir, num - 1) (tail :: tail_locations)
    | _ -> head, tail, tail :: tail_locations
  in
  looper head tail instructions []
;;

let doing_all_instructions head tail all_instructions =
  let rec looper head tail all_instructions tail_locations =
    match all_instructions with
    | [] -> tail_locations
    | hd :: tl ->
      let new_head, new_tail, tails = move_the_rope head tail hd in
      looper new_head new_tail tl (tails :: tail_locations)
  in
  looper head tail all_instructions []
;;

let input = read_lines "inputs/9.txt" |> remove_empty_string |> split_on_space
let head = 0, 0
let tail = 0, 0

let output =
  doing_all_instructions head tail input |> List.flatten |> List.unique |> List.length
;;

let outcome_1 = output
let outcome_2 = 2

let main () =
  Printf.printf "Part 1 total is %d\n" outcome_1;
  Printf.printf "Part 2 total is %d\n" outcome_2
;;
