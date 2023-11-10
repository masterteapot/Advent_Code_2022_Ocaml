(* Round 1 Instructions *)
    (* A for Rock, B for Paper, and C for Scissors *)
    (* X for Rock, Y for Paper, and Z for Scissors *)
    (* The score for a single round is the score for the shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won) *)

(* Round 2 Instructions *)
    (* X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win. Good luck! *)

exception Not_a_strat of string
let filename = "./inputs/2.txt"
type strat = Rock | Paper | Scissors
type strat_keys = {
    name : string;
    value : int;
    strategy : strat
}
type move = {
    win : strat;
    lose : strat;
    draw : strat;
    value : int
}
let rock = {
    win = Scissors;
    lose = Paper;
    draw = Rock;
    value = 1
}
let paper = {
    win = Rock;
    lose = Scissors;
    draw = Paper;
    value = 2
}
let scissors = {
    win = Paper;
    lose = Rock;
    draw = Scissors;
    value = 3
}

let get_opponent_move a_string = 
    match a_string with
    | "A" -> rock
    | "B" -> paper
    | "C" -> scissors
    | _ -> raise (Not_a_strat a_string)

let get_counter_move a_strat =
    match a_strat with
    | Rock -> rock
    | Scissors -> scissors
    | Paper -> paper

let rec print_tuples ls_tuples =
    match ls_tuples with 
    | [] -> ()
    | (a, b) :: rest ->
    Printf.printf "%s, %s;\n" a b;
    print_tuples rest

(* read file line by line *)
let read_lines filename =
    let contents = In_channel.with_open_bin filename In_channel.input_all in
    String.split_on_char '\n' contents

let assign_strategy_v1 a_string =
    match a_string with
    | "A" -> {name = "Rock"; value = 1; strategy = Rock}
    | "B" -> {name = "Paper"; value = 2; strategy = Paper}
    | "C" -> {name = "Scissors"; value = 3; strategy = Scissors}
    | "X" -> {name = "Rock"; value = 1; strategy = Rock}
    | "Y" -> {name = "Paper"; value = 2; strategy = Paper}
    | "Z" -> {name = "Scissors"; value = 3; strategy = Scissors}
    | other -> raise (Not_a_strat other)

let assign_mixed_strategy a_string b_string =
    match a_string with
    | "A" | "B" | "C" -> (
        let o_move = get_opponent_move a_string in
        match b_string with
        | "X" -> let s = (get_counter_move o_move.win) in s.value + 0
        | "Y" -> let s = (get_counter_move o_move.draw) in s.value + 3
        | "Z" -> let s = (get_counter_move o_move.lose) in s.value + 6
        | other -> raise (Not_a_strat other)
    )
    | other -> raise (Not_a_strat other)


let get_mixed_strategy a_tuple =
    match a_tuple with
    | (a, b) -> assign_mixed_strategy a b


let rec split_str_to_tuple input output =
    match input with
        | [] -> output
        | head :: tail -> 
            let ls_strats = String.split_on_char ' ' head in (
                    match ls_strats with
                    | [] -> split_str_to_tuple tail output
                    | _ :: [] -> split_str_to_tuple tail output
                    | fst :: snd :: [] -> split_str_to_tuple tail ((fst, snd) :: output)
                    | fst :: snd :: _ -> split_str_to_tuple tail ((fst, snd) :: output)
                    )

let rec recurse_strategies input output =
    match input with
    | [] -> output
    | hd :: tl -> recurse_strategies tl (
        match hd with
                | (a, b) -> ((assign_strategy_v1 a, assign_strategy_v1 b) :: output)
        )

let evalute_strategies contest =
    match contest with
        | a, b -> (
            if a.strategy = b.strategy then b.value + 3 else
            if ((a.strategy = Rock) && (b.strategy = Scissors)) then b.value else
            if ((a.strategy = Rock) && (b.strategy = Paper)) then b.value + 6 else
            if ((a.strategy = Paper) && (b.strategy = Rock)) then b.value else
            if ((a.strategy = Paper) && (b.strategy = Scissors)) then b.value + 6 else
            if ((a.strategy = Scissors) && (b.strategy = Paper)) then b.value else
            if ((a.strategy = Scissors) && (b.strategy = Rock)) then b.value + 6 else
            0
        )

let line_file = read_lines filename
let grouped_raw_data = split_str_to_tuple line_file []

let main () =
    (* Part 1 *)
    let assigned_strats = recurse_strategies grouped_raw_data [] in
    let ls_points = List.map evalute_strategies assigned_strats in
    let total_points = List.fold_left ( + ) 0 ls_points in
    Printf.printf "\nFinal answer for part 1 is ... %d\n" total_points;

    (* Part 2 *)
    let ls_points_v2 = List.map get_mixed_strategy grouped_raw_data in
    let total_points_v2 = List.fold_left ( + ) 0 ls_points_v2 in
    Printf.printf "\nFinal answer for part 2 is ... %d\n\n" total_points_v2;;

