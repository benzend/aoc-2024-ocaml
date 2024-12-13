let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines;;

let split_by_whitespace s =
  let is_whitespace c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  let rec aux i j acc =
    if j = String.length s then
      if i = j then acc else String.sub s i (j - i) :: acc
    else if is_whitespace s.[j] then
      if i = j then aux (j + 1) (j + 1) acc
      else aux (j + 1) (j + 1) (String.sub s i (j - i) :: acc)
    else
      aux i (j + 1) acc
  in
  List.rev (aux 0 0 []);;


let day_one =
  let lines = read_file "./data/day-1.txt" in
  let split = List.map split_by_whitespace lines in
  let first_of_list lst =
    match lst with
    | [] -> None
    | x :: _ -> Some x in
  let last_of_list lst =
    let rev = List.rev lst in
    first_of_list rev in
  let left = List.map first_of_list split in
  let right = List.map last_of_list split in
  let optional_int_of_string s =
    match s with
    | Some x -> int_of_string x
    | None -> 0 in
  let left_int = List.map optional_int_of_string left in
  let right_int = List.map optional_int_of_string right in
  let left_sorted = List.sort compare left_int in
  let right_sorted = List.sort compare right_int in
  let combined = List.combine left_sorted right_sorted in
  let diff x y = if x > y then x - y else y - x in
  let sum = List.fold_left (fun a (t, b) -> a + diff t b) 0 combined in
  let part_one = string_of_int sum in
  let hash = Hashtbl.create 123456 in
  let setup_hash elem =
    let v = Hashtbl.find_opt hash elem in
    match v with
    | Some x -> Hashtbl.replace hash elem (x + 1)
    | None -> Hashtbl.add hash elem 1 in
  let () = List.iter setup_hash right_int in
  let sum_self_times_count v =
    let s = Hashtbl.find_opt hash v in
    match s with
    | Some x -> v * x
    | None -> 0 in
  let counts = List.map sum_self_times_count left_int in
  let sum = List.fold_left ( + ) 0 counts in
  let part_two = string_of_int sum in
  (part_one, part_two);;


Printf.printf "Day One: Part One: %s, Part Two: %s" (fst day_one) (snd day_one);;

let day_two =
  let lines = read_file "./data/day-2.txt" in
  let reports = List.map split_by_whitespace lines in
  let is_safe report =
    let check x y =
      let x_i = int_of_string x in
      let y_i = int_of_string y in
      if x_i < y_i && y_i - x_i < 4 then
        "+"
      else if x_i > y_i && x_i - y_i < 4 then
        "-"
      else
        ":("
      in
    let rec f l prev_check prev_n =
      if prev_check = ":(" then
        false
      else
        match l with
        | [] -> false
        (* prev_check will never be ":(" thanks to the guard above *)
        | [x] -> if prev_check == "" || (check prev_n x) = prev_check then true else false
        | [x; y] -> if prev_check == "" || (check x y) = prev_check then true else false
        | x :: y :: remainder -> f remainder (check x y) y in
    f report "" "" in
  let safe_reports = List.filter is_safe reports in
  let part_one = List.length safe_reports in
  string_of_int part_one;;


Printf.printf "Day Two: Part One: %s" day_two;;
