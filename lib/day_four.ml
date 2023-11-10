let filename = "./inputs/4.txt"

exception No_match_found of string


(* read file line by line *)
let read_lines filename =
    let contents = In_channel.with_open_bin filename In_channel.input_all in
    String.split_on_char '\n' contents


let rec remove_empty_string a_list =
    match a_list with
        | [] -> []
        | a_str :: tl -> 
            match a_str with
                | "" -> remove_empty_string tl
                | a_str -> a_str :: remove_empty_string tl


let split_ranges elf =
    match elf with
        | e1 :: e2 :: [] -> 
            [List.map int_of_string (String.split_on_char '-' e1) 
            ; List.map int_of_string (String.split_on_char '-' e2)]
        | _ -> raise (No_match_found "Should have had 2 elves with 2 numbers")


let rec clean_inputs a_list =
    match a_list with
    | [] -> [] 
    | hd :: tl -> 
        let elves = (String.split_on_char ',' hd) in
    (split_ranges elves) :: clean_inputs tl


let rec total_overlap a_list =
    match a_list with
    | [] -> []
    | [ [low1; high1]; [low2; high2] ] :: tl ->
        if ( (low1 >= low2) && (high1 <= high2) ) || ( (low2 >= low1) && (high2 <= high1) ) then
            1 :: total_overlap tl
        else 0 :: total_overlap tl
    | _ -> raise (No_match_found "where are the guys?")


let rec any_overlap a_list =
    match a_list with
    | [] -> []
    | [ [low1; high1]; [low2; high2] ] :: tl ->
        if 
           ( (low1 >= low2) && (low1 <= high2) ) 
        || ( (high1 >= low2) && (high1 <= high2) ) 
        || ( (low2 >= low1) && (low2 <= high1) ) 
        || ( (high2 >= low1) && (high2 <= high1) ) 
        then 1 :: any_overlap tl
        else 0 :: any_overlap tl
    | _ -> raise (No_match_found "where are the guys?")


let input = remove_empty_string (read_lines filename)
let teams = clean_inputs input

(* Part 1 *)
let overlapped_elves = total_overlap teams
let pt_1_total = List.fold_left ( + ) 0 overlapped_elves

(* Part 2 *)
let any_overlapped_elves = any_overlap teams
let pt_2_total = List.fold_left ( + ) 0 any_overlapped_elves


let main () = 
    let outcome = pt_1_total in
    Printf.printf "Part 1 total is %d\n" outcome;
    
    let outcome_2 = pt_2_total in
    Printf.printf "Part 2 total is %d\n" outcome_2;;
