let rec string_to_list string =
  match string with
    "" -> []
  | _ -> (String.get string 0)::(string_to_list (String.sub string 1 (String.length string -1)))
;;

open Str
let input =
  let rec readFile (f: in_channel) =
    let current = ref [] in
    try
      let line = ref " " in

      while String.length !line > 0 do
        line := input_line f;
        current := (split (regexp "[ ]+") !line) @ !current;
      done;
      (!current)::(readFile f)
    with End_of_file -> close_in f; [!current] in

  let file = (open_in "input") in
  readFile file
;;

let rec getYesSet yesSet group =
  match group with
    [] -> yesSet
  | mem::rest ->
     let newSet = List.sort_uniq Char.compare (yesSet @ (string_to_list mem)) in
     getYesSet newSet rest
;;

let groupCounts =
  let getGroupCount group = List.length (getYesSet [] group) in
  List.map getGroupCount input
;;

let ans1 = List.fold_left (fun acc x -> acc + x) 0 groupCounts;;

let rec intersect a b =
  match a with
    [] -> []
  | x::rest ->
     if List.mem x b then
       x::(intersect rest b)
     else
       intersect rest b
;;

let getAllYes group =
  let (h::rest) = List.map string_to_list group in
  List.fold_left intersect h rest
;;

let allYes = List.map getAllYes input;;

let ans2 = List.fold_left (fun acc g -> acc + (List.length g)) 0 allYes;;

print_string ("ans1: " ^ (string_of_int ans1));;
print_newline ();;
print_string ("ans2: " ^ (string_of_int ans2));;
print_newline ();;
