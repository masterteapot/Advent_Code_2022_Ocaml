open Batteries

type fil =
  { name : string
  ; size : int
  ; path : string list
  }

type dir =
  { name : string
  ; mutable dirs : dir list
  ; mutable fils : fil list
  ; path : string list
  ; mutable size : int
  }

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

let parse_cd command path =
  let cmd_ls = String.split_on_char ' ' command in
  match cmd_ls with
  | [ "$"; "cd"; "/" ] -> [ "/" ]
  | [ "$"; "cd"; ".." ] -> List.take (List.length path - 1) path
  | [ "$"; "cd"; d_name ] -> path @ [ d_name ]
  | _ -> failwith "Not a cd command in the structure of '$ cd x'"
;;

let construct_dir a_str dirs fils path =
  let split_str = String.split_on_char ' ' a_str in
  let name = List.last split_str in
  { name; dirs; fils; path; size = 0 }
;;

let construct_fil a_str path =
  let split_str = String.split_on_char ' ' a_str in
  let name = List.last split_str in
  let size = int_of_string @@ List.hd split_str in
  { name; path; size }
;;

let rec add_dir_contents (cur_dir : dir) (contents : string list) (path : string list) =
  match contents with
  | [] -> cur_dir
  | hd :: tl ->
    (match hd with
     | d_str when d_str.[0] = 'd' ->
       let in_dir = construct_dir d_str [] [] path in
       let new_dir = { cur_dir with dirs = in_dir :: cur_dir.dirs } in
       add_dir_contents new_dir tl path
     | f_str ->
       let in_fil = construct_fil f_str path in
       let new_dir = { cur_dir with fils = in_fil :: cur_dir.fils } in
       add_dir_contents new_dir tl path)
;;

let rec find_dir a_dir path =
  match path with
  | [] -> a_dir
  | "/" :: [] -> a_dir
  | "/" :: tl -> find_dir a_dir tl
  | hd :: tl -> find_dir (List.find (fun x -> x.name = hd) a_dir.dirs) tl
;;

let update_record_value a_dir path d_dirs f_fils =
  let inner_dir = a_dir in
  let rec helper a_dir path value inner_dir =
    match path with
    | [] ->
      inner_dir.dirs <- d_dirs;
      inner_dir.fils <- f_fils;
      a_dir
    | "/" :: [] ->
      inner_dir.dirs <- d_dirs;
      inner_dir.fils <- f_fils;
      a_dir
    | "/" :: tl -> helper a_dir tl value inner_dir
    | hd :: tl -> helper a_dir tl value (List.find (fun x -> x.name = hd) inner_dir.dirs)
  in
  helper a_dir path d_dirs inner_dir
;;

let update_records (path : string list) (content_acc : string list) (result : dir) =
  let dir_to_update = add_dir_contents (find_dir result path) content_acc path in
  update_record_value result path dir_to_update.dirs dir_to_update.fils
;;

let parse_commands (inputs : string list) =
  let rec inner_commands
    (inputs : string list)
    (path : string list)
    (content_acc : string list)
    (result : dir)
    =
    match inputs with
    | [] -> update_records path content_acc result
    | hd :: tl ->
      (match hd with
       | content when content.[0] <> '$' ->
         inner_commands tl path (content :: content_acc) result
       | early_cd when String.starts_with early_cd "$ cd" && path = [] ->
         inner_commands tl (parse_cd early_cd path) [] result
       | early_ls when String.starts_with early_ls "$ ls" && path = [ "/" ] ->
         inner_commands tl path [] result
       | cd when String.starts_with cd "$ cd" ->
         inner_commands tl (parse_cd cd path) [] (update_records path content_acc result)
       | _ -> inner_commands tl path [] result)
  in
  inner_commands inputs [] [] (construct_dir "dir /" [] [] [])
;;

let calculate_sizes top_dir =
  let sum_fils (a_dir : dir) =
    List.fold_left (fun (x : int) (y : fil) -> x + y.size) 0 a_dir.fils
  in
  let sum_ds (a_dir : dir) =
    List.fold_left (fun (x : int) (y : dir) -> x + y.size) 0 a_dir.dirs
  in
  let rec loop_dirs top_dir (cur_dir : dir) (todo_dirs : dir list) =
    let unmeasured = List.filter (fun (x : dir) -> x.size = 0) cur_dir.dirs in
    let todo_dirs =
      List.filter (fun (x : dir) -> x.size = 0 && x.name <> cur_dir.name) todo_dirs
      @ unmeasured
      |> List.unique
    in
    match cur_dir with
    | d when d.size > 0 && List.is_empty todo_dirs -> d
    | d when List.is_empty d.dirs ->
      d.size <- sum_fils d;
      loop_dirs
        top_dir
        (match todo_dirs with
         | hd :: _ -> hd
         | [] -> top_dir)
        todo_dirs
    | _ when List.length unmeasured > 0 ->
      loop_dirs top_dir (List.hd unmeasured) todo_dirs
    | d ->
      d.size <- sum_fils d + sum_ds d;
      loop_dirs
        top_dir
        (match todo_dirs with
         | hd :: _ -> hd
         | [] -> top_dir)
        todo_dirs
  in
  loop_dirs top_dir top_dir [ top_dir ]
;;

let rec get_all_dirs dirs_to_index indexed_dirs =
  match dirs_to_index with
  | [] -> indexed_dirs
  | hd :: tl -> get_all_dirs (tl @ hd.dirs) (hd :: indexed_dirs)
;;

let filename = "./inputs/7.txt"
let output = read_lines filename |> remove_empty_string |> parse_commands
let output = calculate_sizes output
let all_dirs = get_all_dirs [ output ] []
let small_dirs = List.filter (fun x -> x.size < 100000) all_dirs
let outcome_1 = List.fold_left (fun p i -> p + i.size) 0 small_dirs

(* section 2 *)
let system_size = 70000000
let needed_size = 30000000
let used_space = output.size
let free_space = system_size - used_space
let min_size_folder = needed_size - free_space

let sorted_dirs =
  List.sort
    (fun (x : dir) (y : dir) ->
      match x with
      | x when x.size = y.size -> 0
      | x when x.size > y.size -> 1
      | _ -> -1)
    all_dirs
;;

let recommended_folder_to_delete =
  List.find (fun (x : dir) -> x.size >= min_size_folder) sorted_dirs
;;

let outcome_2 = recommended_folder_to_delete.size

let main () =
  Printf.printf "Part 1 total is %d\n" outcome_1;
  Printf.printf "Part 2 total is %d\n" outcome_2
;;
