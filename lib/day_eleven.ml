open Batteries

type monkey =
  { monkey_num : int
  ; mutable items : int list
  ; stress_eval : int -> int
  ; test : int -> bool
  ; true_monkey : int
  ; false_monkey : int
  ; mutable num_operations : int
  }

type operand =
  { plus : int -> int -> int
  ; minus : int -> int -> int
  ; multiply : int -> int -> int
  ; divide : int -> int -> int
  }

let operation_operand = { plus = ( + ); minus = ( - ); multiply = ( * ); divide = ( / ) }

let read_lines filename =
  let contents = In_channel.with_open_bin filename In_channel.input_all in
  String.split_on_char '\n' contents
;;

let group_monkeys a_list =
  let rec looper a_list monkey all_monkeys =
    match a_list with
    | [] -> List.rev all_monkeys
    | a_str :: tl ->
      (match a_str with
       | "" -> looper tl [] (List.rev monkey :: all_monkeys)
       | a_str -> looper tl (String.strip a_str :: monkey) all_monkeys)
  in
  looper a_list [] []
;;

let parse_monkey stress_reducer index input =
  let rec looper input monkey =
    match input with
    | [] -> monkey
    (* Get the starting items *)
    | hd :: tl when String.starts_with hd "Starting items" ->
      let items = String.split_on_char ':' hd in
      let items =
        match items with
        | _ :: items :: _ -> String.split_on_char ',' items
        | _ -> failwith "didn't find expected items structure"
      in
      looper
        tl
        { monkey with items = List.map (fun x -> int_of_string (String.strip x)) items }
    (* Get the test func *)
    | hd :: tl when String.starts_with hd "Test:" ->
      let dv = String.split_on_char ' ' hd |> List.last in
      looper tl { monkey with test = (fun x -> Int.rem x (int_of_string dv) = 0) }
    (* Get the true monkey *)
    | hd :: tl when String.starts_with hd "If true:" ->
      let tm = String.split_on_char ' ' hd |> List.last |> int_of_string in
      looper tl { monkey with true_monkey = tm }
    (* Get the false monkey *)
    | hd :: tl when String.starts_with hd "If false:" ->
      let fm = String.split_on_char ' ' hd |> List.last |> int_of_string in
      looper tl { monkey with false_monkey = fm }
    (* Get the operand *)
    | hd :: tl when String.starts_with hd "Operation: " ->
      let ops = String.split_on_string ~by:"new = " hd |> List.last in
      let ops = String.split_on_char ' ' ops in
      let stress_operand =
        match List.nth ops 1 with
        | "+" -> operation_operand.plus
        | "*" -> operation_operand.multiply
        | "/" -> operation_operand.divide
        | "-" -> operation_operand.minus
        | _ -> failwith "missing the opperand"
      in
      let operation_constant =
        match List.last ops with
        | "old" -> 0
        | right -> int_of_string right
      in
      let operation_on_constant = operation_constant <> 0 in
      let stress_eval =
        if operation_on_constant = true
        then fun x -> stress_reducer (stress_operand x operation_constant)
        else fun x -> stress_reducer (stress_operand x x)
      in
      looper tl { monkey with stress_eval }
    | _ :: tl -> looper tl monkey
  in
  looper
    input
    { monkey_num = index
    ; items = []
    ; num_operations = 0
    ; stress_eval = (fun x -> x + x)
    ; test = (fun x -> x > 0)
    ; true_monkey = 0
    ; false_monkey = 0
    }
;;

let rec monkey_index counter monkeys =
  match monkeys with
  | [] -> []
  | _ :: tl -> counter :: monkey_index (counter + 1) tl
;;

let monkey_business rounds monkey_index monkeys =
  let rec monkey_math index monkeys =
    let my_monkey = List.nth monkeys index in
    match my_monkey.items with
    | [] -> monkeys
    | item :: tl ->
      let stress = my_monkey.stress_eval item in
      Printf.printf "Item stress is: %d\n" stress;
      let result = my_monkey.test stress in
      let new_monkey_index =
        if result = true then my_monkey.true_monkey else my_monkey.false_monkey
      in
      my_monkey.items <- tl;
      my_monkey.num_operations <- my_monkey.num_operations + 1;
      let new_monkey = List.nth monkeys new_monkey_index in
      new_monkey.items <- stress :: new_monkey.items;
      monkey_math index monkeys
  in
  let rec monkey_loop monkey_index monkeys =
    match monkey_index with
    | [] -> monkeys
    | i :: tl ->
      let monkeys = monkey_math i monkeys in
      monkey_loop tl monkeys
  in
  let rec round_loop rounds counter monkey_index monkeys =
    match counter with
    | _ when counter > rounds -> monkeys
    | _ ->
      let monkeys = monkey_loop monkey_index monkeys in
      round_loop rounds (counter + 1) monkey_index monkeys
  in
  round_loop rounds 1 monkey_index monkeys
;;

let input =
  read_lines "inputs/11_test.txt"
  |> group_monkeys
  |> List.filter (fun x -> List.length x > 0)
;;

let input_1 = List.mapi (parse_monkey (fun x -> x / 3)) input
let mi_1 = monkey_index 0 input_1
let final_1 = monkey_business 20 mi_1 input_1

let top_2_v1 =
  List.sort
    (fun x y ->
      match x, y with
      | _ when x.num_operations > y.num_operations -> -1
      | _ when x.num_operations = y.num_operations -> 0
      | _ -> 1)
    final_1
;;

let top_2_filtered_v1 =
  match top_2_v1 with
  | x :: y :: _ -> [ x; y ]
  | _ -> failwith "Not enough monkeys"
;;

let monkey_score_1 =
  (List.first top_2_filtered_v1).num_operations
  * (List.last top_2_filtered_v1).num_operations
;;

let outcome_1 = monkey_score_1
let input_2 = List.mapi (parse_monkey (fun x -> Int32.of_int x |> Int32.to_int)) input
let mi_2 = monkey_index 0 input_2
let final_2 = monkey_business 20 mi_2 input_2

let top_2_v2 =
  List.sort
    (fun x y ->
      match x, y with
      | _ when x.num_operations > y.num_operations -> -1
      | _ when x.num_operations = y.num_operations -> 0
      | _ -> 1)
    final_2
;;

let top_2_filtered_2 =
  match top_2_v2 with
  | x :: y :: _ -> [ x; y ]
  | _ -> failwith "Not enough monkeys"
;;

let monkey_score_2 =
  (List.first top_2_filtered_2).num_operations
  * (List.last top_2_filtered_2).num_operations
;;

let outcome_2 = monkey_score_2

let () =
  List.iter
    (fun x ->
      Printf.printf "Monkey %d inspected items %d times.\n" x.monkey_num x.num_operations)
    final_2
;;

let main () =
  Printf.printf "Part 1 total is %d\n" outcome_1;
  Printf.printf "Part 2 total is %d\n" outcome_2
;;
