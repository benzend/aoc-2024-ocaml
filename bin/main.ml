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
  string_of_int sum;;

Printf.printf "Day One: %s" day_one;;
