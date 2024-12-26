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

let get_day day = read_file (Printf.sprintf "./data/day-%i.txt" day);;

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

let drop_last ls = ls |> List.rev |> List.tl |> List.rev

let explode_string s = List.init (String.length s) (String.get s)
