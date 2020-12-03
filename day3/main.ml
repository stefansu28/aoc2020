type mapElement =
  Tree
| Empty;;

let rec string_to_list string =
  match string with
    "" -> []
  | _ -> (String.get string 0)::(string_to_list (String.sub string 1 (String.length string -1)))
;;

let input =
  let rec readFile (f: in_channel) =
    try
      let line = input_line f in
      let mapRow = List.map (fun char ->
                       match char with
                         '.' -> Empty
                       | '#' -> Tree
                     ) (string_to_list line) in
      (mapRow)::(readFile f)
      (* (string_to_list line)::(readFile f) *)
    with End_of_file -> close_in f; [] in

  let file = (open_in "input") in
  readFile file
;;

let rec toboggan acc x map =
    match map with
      [] -> acc
    | row::rest ->
       let new_x = (x + 3) mod (List.length row) in
       let new_acc = acc + (
           match (List.nth row new_x) with
             Empty -> 0
           | Tree -> 1) in
       toboggan new_acc new_x rest
;;

let ans1 =
  let _::map = input in
  toboggan 0 0 map
;;

let rec toboggan2 (run, drop) acc x map =
  let current_drop = ref 0 in
  let filtered = List.filter (fun x ->
                     let prev = !current_drop in
                     current_drop := !current_drop + 1;
                     if prev < drop then false else true) map in
  match filtered with
    [] -> acc
  | row::_ ->
     let new_x = (x + run) mod (List.length row) in
     let new_acc = acc + (
         match (List.nth row new_x) with
           Empty -> 0
         | Tree -> 1) in
     toboggan2 (run, drop) new_acc new_x filtered

;;

let ans2 =
  let tobogganMapper slope = toboggan2 slope 0 0 input in
  let slopes = [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)] in
  let hits = List.map tobogganMapper slopes in
  List.fold_left (fun acc x -> acc * x) 1 hits
;;

print_int ans1;
print_newline ();
print_int ans2;
print_newline ()
