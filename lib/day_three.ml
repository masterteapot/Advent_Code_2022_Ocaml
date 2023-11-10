let filename = "./inputs/3.txt"

exception No_match_found of string

(* read file line by line *)
let read_lines filename =
    let contents = In_channel.with_open_bin filename In_channel.input_all in
    String.split_on_char '\n' contents

let get_first_half a_str =
    let half_len = String.length a_str / 2 in
    String.sub a_str 0 half_len
    
let get_second_half a_str =
    let half_len = String.length a_str / 2 in
    String.sub a_str half_len half_len

let get_prio a_char =
    let az = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" in
    match a_char with
        | '.' -> 0
        | a_char -> String.index az a_char + 1

let remove_duplicates a_string =
    let rec traverse_chars b_string result =
    begin
        match b_string with
        | "" -> ""
        | b_string -> begin
                let raw_char = (b_string.[0]) in
                let clean_char = if (String.contains result raw_char) then "" else Char.escaped raw_char in
                let shorty = String.sub b_string 1 (String.length b_string - 1) in
                let new_result = clean_char ^ result in
               clean_char ^ traverse_chars shorty new_result 
        end
    end in
    traverse_chars a_string ""


let rec non_unique_char a_string b_string =
    match a_string with
    | "" -> '.'
    | a -> begin
        if String.contains b_string a.[0] then a.[0] 
        else non_unique_char (String.sub a_string 1 (String.length a_string - 1)) b_string
    end

let rec evaluate_input a_list output =
    match a_list with
    | [] -> output
    | hd :: tl -> begin
        let fh = get_first_half hd in
        let sh = get_second_half hd in
        let matched_value = get_prio (non_unique_char fh sh) in
        evaluate_input tl (matched_value :: output)
    end

let rec remove_empty_string a_list =
    match a_list with
        | [] -> []
        | a_str :: tl -> 
        begin
            if a_str = "" then
                remove_empty_string tl
            else
                a_str :: remove_empty_string tl
        end

let rec group_three in_list =
    match in_list with
        | a :: b :: c :: tl -> [
            remove_duplicates a; 
            remove_duplicates b; 
            remove_duplicates c] :: group_three tl
        | _ -> []

let rec three_matching_char a_string b_string c_string =
    match a_string with
    | "" -> '.'
    | a -> begin
        if (String.contains b_string a.[0])
            && (String.contains c_string a.[0]) then a.[0] 
        else three_matching_char (String.sub a_string 1 (String.length a_string - 1)) b_string c_string
    end

let rec match_three_strings a_list output =
    match a_list with
    | [] -> output
    | hd :: tl -> begin
        match hd with
        | a :: b :: c :: [] -> 
            match_three_strings 
                tl 
                (get_prio (three_matching_char a b c) :: output)
        | _ -> output
        end

let main () =
    (* Part 1 *)
    let input = remove_empty_string (read_lines filename) in
    let results = evaluate_input input [] in
    let total = List.fold_left ( + ) 0 results in

    (* Part 2 *)
    let grouped_list = group_three input in
    let result_list = match_three_strings grouped_list [] in
    let total_two = List.fold_left ( + ) 0 result_list in

    Printf.printf "\nPart 1 total = %d\n" total;
    Printf.printf "Part 2 total = %d\n\n" total_two;;
