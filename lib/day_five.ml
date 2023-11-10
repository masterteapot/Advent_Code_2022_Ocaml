let filename = "./inputs/5.txt"
exception No_match_found of string

(* read file line by line *)
let read_lines filename =
    let contents = In_channel.with_open_bin filename In_channel.input_all in
    String.split_on_char '\n' contents


let find_first_empty_string a_list = 
    let rec find_an_empty x_list counter =
    match x_list with
        | [] -> raise (No_match_found "No empty string found in list")
        | "" :: _ -> counter
        | _ :: tl -> find_an_empty tl (counter + 1) in
    find_an_empty a_list 0

let input = read_lines filename
let empty_index = find_first_empty_string input

let pt_1_text = "CMV"
let pt_2_text = "CMV"

let main () = 
    let outcome = pt_1_text in
    Printf.printf "Part 1 total is %s\n" outcome;
    
    let outcome_2 = pt_2_text in
    Printf.printf "Part 2 total is %s\n" outcome_2;;
