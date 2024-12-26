let lines = Util.get_day 1
let split = List.map Util.split_by_whitespace lines
let first_of_list lst =
  match lst with
  | [] -> None
  | x :: _ -> Some x

let last_of_list lst =
  let rev = List.rev lst in
  first_of_list rev

let left = List.map first_of_list split
let right = List.map last_of_list split
let optional_int_of_string s =
  match s with
  | Some x -> int_of_string x
  | None -> 0
let left_int = List.map optional_int_of_string left
let right_int = List.map optional_int_of_string right
let left_sorted = List.sort compare left_int
let right_sorted = List.sort compare right_int
let combined = List.combine left_sorted right_sorted
let diff x y = if x > y then x - y else y - x
let sum = List.fold_left (fun a (t, b) -> a + diff t b) 0 combined
let part_one = string_of_int sum
let hash = Hashtbl.create 123456
let setup_hash elem =
  let v = Hashtbl.find_opt hash elem in
  match v with
  | Some x -> Hashtbl.replace hash elem (x + 1)
  | None -> Hashtbl.add hash elem 1
let () = List.iter setup_hash right_int
let sum_self_times_count v =
  let s = Hashtbl.find_opt hash v in
  match s with
  | Some x -> v * x
  | None -> 0

let counts = List.map sum_self_times_count left_int
let sum = List.fold_left ( + ) 0 counts

let part_two = string_of_int sum
