open Batteries

type monkey =
  { mutable items : int list
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

let parse_monkey input =
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
        then fun x -> stress_operand x operation_constant / 3
        else fun x -> stress_operand x x / 3
      in
      looper tl { monkey with stress_eval }
    | _ :: tl -> looper tl monkey
  in
  looper
    input
    { items = []
    ; num_operations = 0
    ; stress_eval = (fun x -> x + x)
    ; test = (fun x -> x > 0)
    ; true_monkey = 0
    ; false_monkey = 0
    }
;;

let input =
  read_lines "inputs/11_test.txt"
  |> group_monkeys
  |> List.filter (fun x -> List.length x > 0)
;;

let test_monkey = List.hd input
let test_output = parse_monkey test_monkey
let outcome_1 = 1
let outcome_2 = 2

let main () =
  Printf.printf "Part 1 total is %d\n" outcome_1;
  Printf.printf "Part 2 total is %d\n" outcome_2
;;
