let lines = Util.get_day 2
let reports = List.map Util.split_by_whitespace lines
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
  f report "" ""
let safe_reports = List.filter is_safe_report reports
let part_one = List.length safe_reports
let validate line cmp =
  match line with
  | [] -> false
  | l ->
  List.fold_left2
    (fun acc a b -> acc && cmp a b && Int.abs (a - b) < 4 && Int.abs (a - b) > 0)
    true (Util.drop_last l) (List.tl l)

let explode line =
    let rec go past rem acc =
        match rem with
            | [] -> acc
            | hd :: tl -> go (hd :: past) tl (List.append (List.rev past) tl :: acc)
    in
        line :: go [] line []

let is_decreasing line = validate line ( > )
let is_increasing line = validate line ( < )
let is_decreasing_2 line = List.map is_decreasing (explode line) |> List.fold_left ( || ) false
let is_increasing_2 line = List.map is_increasing (explode line) |> List.fold_left ( || ) false

let solve input filter =
  input
    |> List.map (fun line -> line |> String.split_on_char ' ' |> List.map int_of_string)
    |> List.filter filter |> List.length |> string_of_int
let safe_reports = List.filter is_safe_report reports
let part_two = solve (Util.get_day 2) (fun l -> is_increasing_2 l || is_decreasing_2 l)
(* let part_two = Util.get_day 2 |> List.length |> string_of_int *)
