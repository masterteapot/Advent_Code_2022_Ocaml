let filename = "./inputs/1.txt"

let rec count_calories text_stream total_cal ls_totals =
    try 
        let line = input_line text_stream in
        match line with
        | "" -> count_calories text_stream 0 (total_cal :: ls_totals)
        | new_val -> count_calories text_stream ((int_of_string new_val) + total_cal) ls_totals
    with _ ->
        ls_totals


let print_list ls =
    let p_fun elem = Printf.printf "%d \n" elem in
    List.iter p_fun ls


(* let get_max val_1 val_2 = *)
(*     if val_1 >= val_2 then val_1 *)
(*     else val_2 *)


(* let rec max_of_list ls max_val = *)
(*     match ls with *)
(*         | [] -> max_val *)
(*         | hd :: ls -> max_of_list ls (get_max hd max_val) *)


let swap_top_three val_1 top_three =
    if 
        List.length top_three < 3 then val_1 :: top_three
    else
        let sorted_three = List.sort compare top_three in
        match sorted_three with
        | [a; b; c] -> if val_1 > a then [val_1; b; c] else [a; b; c]
        | _ -> []
 

let sum_list = List.fold_left ( + ) 0


let rec get_max_three ls top_three =
    match ls with
        | [] -> top_three
        | hd :: ls -> get_max_three ls (swap_top_three hd top_three)


let main () = 
    let text_stream = (In_channel.open_text filename) in
    let list_of_calories = count_calories text_stream 0 [] in
    let top_three_elves = get_max_three list_of_calories [] in
    let top_three_total = sum_list top_three_elves in
    Printf.printf "\nThe max calories are... %d\n\n" top_three_total;
    flush stdout;
    close_in text_stream;;

