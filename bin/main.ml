Printf.printf "Day One: Part One: %s, Part Two: %s\n"
Aoc_2024_ocaml.Day_1.part_one Aoc_2024_ocaml.Day_1.part_two;;

Printf.printf "Day Two: Part One: %i, Part Two: %s\n"
Aoc_2024_ocaml.Day_2.part_one Aoc_2024_ocaml.Day_2.part_two;;

Printf.printf "Day 15: Part One: %i"
(Aoc_2024_ocaml.Day_15.solve (Aoc_2024_ocaml.Util.get_day 15) false);;

Printf.printf "Day 15: Test: %i"
(Aoc_2024_ocaml.Day_15.solve [
  "########";
  "#..O.O.#";
  "##@.O..#";
  "#...O..#";
  "#.#.O..#";
  "#...O..#";
  "#......#";
  "########";
  "";
  "<^^>>>vv<v>>v<<"
] true);;
