let rec string_to_list string =
  match string with
    "" -> []
  | _ -> (String.get string 0)::(string_to_list (String.sub string 1 (String.length string -1)))
;;

let input =
  let rec readFile (f: in_channel) =
    try
      let line = input_line f in
      (string_to_list line)::(readFile f)
    with End_of_file -> close_in f; [] in

  let file = (open_in "input") in
  readFile file
;;

let rec getPos (min, max) (low, high) pass =
  match pass with
    x::rest when x = low -> getPos (min, min + (max - min)/2) (low, high) rest
  | x::rest when x = high -> getPos (min + (max - min + 1)/2, max) (low, high) rest
  | _::rest when min != max -> failwith "encountered value that is neither low or high"
  | _ -> min, pass

let seats =
  List.map (fun pass ->
      let row, rest = getPos (0, 127) ('F', 'B') pass in
      let col, rest = getPos (0, 7) ('L', 'R') rest in
      (row, col)
    ) input

let seatIds = List.map (fun (row, col) -> row * 8 + col) seats

let ans1 = List.fold_left (fun max x -> if max < x then x else max) 0 seatIds;;

let minId = List.fold_left (fun min x -> if min > x then x else min) ans1 seatIds;;

let gauss n = n * (n + 1) / 2;;

let high = gauss ans1;;
let low = gauss (minId - 1);;
let expected = high - low;;
let sum = List.fold_left (fun acc x -> acc + x) 0 seatIds;;

let ans2 = expected - sum;;

print_string ("ans1: " ^ (string_of_int ans1));;
print_newline ();;
print_string ("ans2: " ^ (string_of_int ans2));;
print_newline ();;
