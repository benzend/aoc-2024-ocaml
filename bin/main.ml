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


Printf.printf "Day One: Part One: %s, Part Two: %s\n" (fst day_one) (snd day_one);;

let day_two l =
  let lines = match l with
  | Some l -> l
  | None ->read_file "./data/day-2.txt" in
  let reports = List.map split_by_whitespace lines in
  let is_safe_report report =
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
        | [] -> false (* this will never run *)
        (* prev_check will never be ":(" thanks to the guard above *)
        | [x] -> if prev_check = "" || (check prev_n x) = prev_check then true else false
        | x :: remainder -> if prev_check = "" && prev_n == "" then
            f remainder "" x
          else if prev_check = "" && prev_n != "" then
            f remainder (check prev_n x) x
          else if (check prev_n x) = prev_check then
            f remainder (check prev_n x) x
          else
            false
  in
    f report "" "" in
  let safe_reports = List.filter is_safe_report reports in
  let part_one = List.length safe_reports in
  (string_of_int part_one, safe_reports);;

(* let _day_2_test = *)
(*   let lines = ["7 6 4 2 1"; "1 2 7 8 9"; "9 7 6 2 1"; "1 3 2 4 5"; "8 6 4 4 1"; "1 3 6 7 9"] in *)
(*   let answer = "2" in *)
(*   let day_two_res = day_two (Some lines) in *)
(*   let pass_or_fail = if fst day_two_res = answer then *)
(*     "pass" *)
(*   else *)
(*     "fail" *)
(*   in *)
(*   let () = Printf.printf "Day two test: %s. Answer was '%s'. Expected '%s'\n." pass_or_fail (fst day_two_res) answer in *)
(*   let print_report r = *)
(*     let () = Printf.printf "report: " in *)
(*     let print_item s = *)
(*        Printf.printf "%s," s in *)
(*     List.iter print_item r in *)
(*   List.iter print_report (snd day_two_res) *)


let day_two_res = day_two None;;

Printf.printf "Day Two: Part One: %s\n" (fst day_two_res);;
