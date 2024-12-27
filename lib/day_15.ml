(*
##########
#.O.O.OOO#
#........#
#OO......#
#OO@.....#
#O#.....O#
#O.....OO#
#O.....OO#
#OO....OO#
##########
 *)

(*
if < then
  get @ pos
  box_locs = check_left_of @
  check if cant_move @ by boxes
  if cant_move
    do_nothing
  else
    if left_pos_is_block
      move_block
    else
      hmm
move might need to be recursive

each move action, there needs to be a check if there are other
move actions, because if @ moves to the left, it'll move a block if it's
right next to it. And, if there's a block next to that, it'll ahve to get
moved to the left as well, and if there's a wall, than all of the actions
have to stop and prevent the @ from moving

let next_of x d =
  pos = current_pos_of x

  add some numbers to get to the next position
  grab the item based on the position
  next_pos = add_to_get_next_position pos d

let rec move x d =
  next_of = check_next_of x d
  match next_of with
  | None -> ok_lets_move
  | Some b -> move b d
*)

type field_item =
  | Main
  | Box
  | Wall
  | Empty

let field_item_to_string = function
  | Main -> "@"
  | Box -> "O"
  | Wall -> "#"
  | Empty -> "."

let field_item_to_char = function
  | Main -> '@'
  | Box -> 'O'
  | Wall -> '#'
  | Empty -> '.'

let string_to_field_item = function
  | "@" -> Main
  | "O" -> Box
  | "#" -> Wall
  | "." -> Empty
  | s -> Printf.sprintf "%s isn't a valid field item" s |> failwith

let char_to_field_item = function
  | '@' -> Main
  | 'O' -> Box
  | '#' -> Wall
  | '.' -> Empty
  |  c -> Printf.sprintf "%c isn't a valid field item" c |> failwith

type direction =
  | Up
  | Down
  | Left
  | Right

let direction_to_string = function
  | Up -> "^"
  | Down -> "v"
  | Left -> "<"
  | Right -> ">"

let string_to_direction = function
  | "^" -> Up
  | "v" -> Down
  | "<" -> Left
  | ">" -> Right
  | s -> Printf.sprintf "%s is not a valid direction" s |> failwith

let char_to_direction = function
  | '^' -> Up
  | 'v' -> Down
  | '<' -> Left
  | '>' -> Right
  | c -> Printf.sprintf "%c is not a valid direction" c |> failwith

type location = Column of int | Row of int

let location_to_int = function
  | Column n -> n
  | Row n -> n

let ints_to_coordinate ints =
  match ints with
  | (n, m) -> (Row n, Column m)

let coordinate_to_ints coord =
  match coord with
  | (Row n, Column m) -> (n, m)
  | _ -> failwith "invalid coordinate"

let row_coordinate_to_int coord =
  match coord with
  | (Row n, _) -> n
  | _ -> failwith "invalid coordinate"

let column_coordinate_to_int coord =
  match coord with
  | (_, Column m) -> m
  | _ -> failwith "invalid coordinate"

let left_of_coordinate coord =
  match coord with
  | (row, Column m) -> (row, Column (m - 1))
  | _ -> failwith "invalid_coordinate"

let right_of_coordinate coord =
  match coord with
  | (row, Column m) -> (row, Column (m + 1))
  | _ -> failwith "invalid_coordinate"

let up_of_coordinate coord =
  match coord with
  | (Row n, col) -> (Row (n - 1), col)
  | _ -> failwith "invalid_coordinate"

let down_of_coordinate coord =
  match coord with
  | (Row n, col) -> (Row (n + 1), col)
  | _ -> failwith "invalid_coordinate"

let next_coordinate direction coord =
  match direction with
  | Up -> up_of_coordinate coord
  | Left -> left_of_coordinate coord
  | Right -> right_of_coordinate coord
  | Down -> down_of_coordinate coord

let print_coordinate x =
  match x with
  | (Row n, Column m) -> Printf.printf "Row: %i, Column: %i\n" n m
  | _ -> failwith "not a valid coordinate"

let get_map_and_queue data =
  let rec f l r rem is_right =
      match rem with
      | [] -> (List.rev l, List.rev r)
      | x :: rem ->
          if x = "" then
            f l r rem true
          else
            if is_right then
              f l (x :: r) rem is_right
            else
              f (x :: l) r rem is_right
    in
  let (map_lines, queue_lines) = f [] [] data false in

  let convert f row =
    let rec m rem acc =
      match rem with
      | [] -> acc
      | [x] -> (f x) :: acc
      | x :: rem ->
          m rem (f x :: acc) in

    List.map (fun l -> m (List.rev (Util.explode_string l)) []) row
  in

  let map = convert char_to_field_item map_lines in
  let queue = List.flatten (convert char_to_direction queue_lines) in
  (map, queue)

let (map, queue) = get_map_and_queue (Util.get_day 15)

let debug_row row =
  let () = List.iter (fun item -> Printf.printf "%s" (field_item_to_string item)) row in
  Printf.printf "\n"

let debug_map map =
  let () = List.iter debug_row map in
  Printf.printf "\n"

let debug_queue_item item =
  direction_to_string item |> Printf.printf "%s"

let debug_queue q =
  List.iter debug_queue_item q

(*
so, there are a couple states that you have to think about in order for this to work correctly
first, let's define what gets affected when you move a single step:
  the row OR column
this means that we have scoped this down a bit more

we should break this down also a bit more to a step level:
  possible steps:
    - nothing is in the way, (move)
    - a wall is the the way, (do nothing)
    - a block is in the way, (check) check the next area
      - nothing is in the way, (move) let's move all of the items over
      - a wall is in the way, (do nothing)
      - a block is in the way, (check) check next area
        - a wall is in the way, (do nothing)
        - a block is in the way, (check)... (recursive pattern)
        - nothing is in the way, (move) let's move all of the items over


now we have
- move
- do nothing
- check (recursive)

how do we handle the items to move over? can we do this one step at a time?

we have a couple of options:
  - accrue each item into a list
  - recursively look through the list, map indexes of the items that need to be moved
 *)

(*returns updated map*)
let swap_item_in_map new_item coord map =
  let matches row_index col_index coord =
    Row row_index = fst coord && Column col_index = snd coord in

  List.mapi (fun row_index row ->
    List.mapi (fun col_index old_item ->
      if matches row_index col_index coord then new_item else old_item
    ) row
  ) map

let look_for item map =
  List.find_mapi (fun row_index row ->
    match List.find_mapi (fun col_index x ->
      if x = item then
        Some col_index
      else None
    ) row with Some col_index -> Some (Row row_index, Column col_index) | None -> None
  ) map

let what_is_in coord map =
  List.find_mapi (fun row_index row ->
    List.find_mapi (fun col_index item ->
      if Row row_index = fst coord && Column col_index = snd coord then
        Some item
      else
        None
    ) row
  ) map

let what_is_next_of coord direction map =
  match direction with
  | Up -> what_is_in (up_of_coordinate coord) map
  | Left -> what_is_in (left_of_coordinate coord) map
  | Right -> what_is_in (right_of_coordinate coord) map
  | Down -> what_is_in (down_of_coordinate coord) map

let debug_what_is_in coord map =
  let item = what_is_in coord map in
  match item with
  | Some x -> Printf.printf "item: %s" (field_item_to_string x)
  | None -> Printf.printf "no item in coordinate"

(*should return a new map*)
let move direction map =
  (*return items to move, if any*)
  let rec mv direction curr history map =
    match curr with
    | (Wall, _) -> [] (* return empty list to move *)
    | (Empty, _) -> history (* we're going to move these items *)
    | (_, coord) ->
        match what_is_next_of coord direction map with
        | None -> failwith "there should be something here"
        | Some next ->
            mv direction (next, next_coordinate direction coord) (curr :: history) map
  in

  let main_pos = match look_for Main map with
  | Some pos -> pos
  | None -> failwith "where the hell did main go?" in

  let items_to_move = mv direction (Main, main_pos) [] map in

  let move_item_in_map new_map item_to_move =
    let map = if (fst item_to_move) = Main then
      swap_item_in_map Empty (snd item_to_move) new_map
    else
      new_map
    in
    swap_item_in_map (fst item_to_move) (next_coordinate direction (snd item_to_move)) map
  in

  List.fold_left move_item_in_map map items_to_move

let run_queue new_map direction =
  move direction new_map

let find_all_coords_of item map =
  let valid_coords = List.mapi (fun row_index row ->
    List.mapi (fun col_index x ->
      if x = item then Some (Row row_index, Column col_index) else None
    ) row
  ) map in

  let filtered = List.map (fun l ->
    (List.filter_map (fun item -> item) l)
  ) valid_coords in

  List.flatten filtered |> List.map (fun x -> x)

let sum_of_coords coords =
  List.fold_left (fun acc coord ->
    match coord with
    | (Row n, Column m) -> acc + ((100 * n) + m)
    | _ -> failwith "invalid coord"
  ) 0 coords

let solve data debug =
  let (map, queue) = get_map_and_queue data in
  if debug then debug_map map;
  let map_aftermath = List.fold_left run_queue map queue in
  if debug then debug_map map_aftermath;
  map_aftermath |> find_all_coords_of Box |> sum_of_coords
