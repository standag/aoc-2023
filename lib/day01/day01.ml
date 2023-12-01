let input_data = Input.read_input "day01.txt"

let lines = String.split_on_char '\n' input_data

let get_last_number (line : string) : int =
  String.fold_left
    (fun acc c ->
      if Char.code c < 58 && Char.code c > 48 then Char.code c - 48 else acc )
    0 line

let get_first_number (line : string) : int =
  String.fold_left
    (fun acc c ->
      if Char.code c < 58 && Char.code c > 48 && acc = 0 then Char.code c - 48
      else acc )
    0 line

let extract_numbers (line : string) : int =
  (get_first_number line * 10) + get_last_number line

let%test _ = extract_numbers "1abc2" = 12

let numbers = List.map extract_numbers lines

let result = List.fold_left ( + ) 0 numbers

let numbers_table = Hashtbl.create 10

let () = Hashtbl.add numbers_table "one" 1

let () = Hashtbl.add numbers_table "two" 2

let () = Hashtbl.add numbers_table "thr" 3

let () = Hashtbl.add numbers_table "fou" 4

let () = Hashtbl.add numbers_table "fiv" 5

let () = Hashtbl.add numbers_table "six" 6

let () = Hashtbl.add numbers_table "sev" 7

let () = Hashtbl.add numbers_table "eig" 8

let () = Hashtbl.add numbers_table "nin" 9

let number_regex =
  Str.regexp
    "one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine\\|[1-9]"

let get_number (line : string) : int =
  Char.code (String.get line 0)
  |> fun first ->
  if first > 48 && first < 58 then first - 48
  else Hashtbl.find numbers_table line

let get_first_number_2 (line : string) : int =
  Str.search_forward number_regex line 0
  |> fun position -> String.sub (line ^ "  ") position 3 |> get_number

let get_last_number_2 (line : string) : int =
  Str.search_backward number_regex line (String.length line)
  |> fun position -> String.sub (line ^ "  ") position 3 |> get_number

let extract_numbers_2 (line : string) : int =
  (* let line = line in
     let () = Printf.printf "line: %s\n" line in *)
  if String.equal line "" then 0
  else (get_first_number_2 line * 10) + get_last_number_2 line

let%test _ = extract_numbers_2 "two1nine" = 29

let%test _ = extract_numbers_2 "4nineeightseven2" = 42

let numbers2 = List.map extract_numbers_2 lines

let result2 = List.fold_left ( + ) 0 numbers2
